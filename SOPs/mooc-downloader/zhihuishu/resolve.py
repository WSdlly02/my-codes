#!/usr/bin/env python3
"""枚举智慧树课程的章节树与媒体地址（借已登录浏览器的会话，不自己造签名）。

前提：Chrome 已用 --remote-debugging-port=9222 启动，且已登录智慧树、打开课程页。

用法：
    # 0) 只挂上去看看，报告找到了哪个页面
    uv run --with 'playwright>=1.40,<2' python resolve.py

    # 1) 抓一次页面自身的 XHR（会 reload 一次页面），落盘到 xhr/
    uv run --with 'playwright>=1.40,<2' python resolve.py --reload --sniff

    # 2) 用页面上下文直接打接口，判定"是否只靠 Cookie 就能枚举"（关键实验）
    uv run --with 'playwright>=1.40,<2' python resolve.py --probe-api

    # 3) 从抓到的 XHR 里抽章节树
    uv run --with 'playwright>=1.40,<2' python resolve.py --tree
"""
from __future__ import annotations

import argparse
import json
import os
import re
import sys

try:
    from playwright.sync_api import sync_playwright
except ImportError:
    sys.exit("[!] 需要 playwright:  uv run --with 'playwright>=1.40,<2' python resolve.py ...")

ROOT = os.path.dirname(os.path.abspath(__file__))
XHR_DIR = os.path.join(ROOT, "xhr")
os.makedirs(XHR_DIR, exist_ok=True)
ALL_JSONL = os.path.join(XHR_DIR, "all.jsonl")

API_HOST = "studyservice-api.zhihuishu.com"
# 从 studyvideoh5 的 JS bundle 里挖出来的字面量（候选，需实测确认）
CANDIDATES = [
    ("/gateway/t/v1/learning/videolist", "videolist"),
    ("/gateway/t/v1/learning/queryCourse", "queryCourse"),
    ("/gateway/t/v1/learning/queryCourseDispMode", "queryCourseDispMode"),
]

# 只记录这些，避免把图片/CDN 噪音写进来
KEEP = re.compile(r"(studyservice-api|gateway/t/v|videolist|queryCourse|learning/|zhihuishu)", re.I)
MEDIA = re.compile(r"https?://[^\s\"'\\]+\.(?:mp4|m3u8|flv)[^\s\"'\\]*", re.I)


def pick_page(ctx, course_id: str | None):
    pages = [pg for pg in ctx.pages if pg.url.startswith("http")]
    if course_id:
        for pg in pages:
            if course_id in pg.url:
                return pg
    for pg in pages:
        if "/stuStudy" in pg.url:
            return pg
    return None


def attach_sniffer(page):
    def on_response(resp):
        url = resp.url
        if not KEEP.search(url):
            return
        try:
            req = resp.request
            rec = {
                "url": url,
                "method": req.method,
                "status": resp.status,
                "req_headers": dict(req.headers),
                "req_post": (req.post_data or "")[:2000],
                "resp_headers": dict(resp.headers),
            }
            try:
                # 章节树可能很大，别截断（曾经用 20000 导致大课程清单不完整）
                rec["resp_body"] = resp.text()[:8_000_000]
            except Exception as e:
                rec["resp_body"] = f"<unreadable: {e}>"
        except Exception as e:
            rec = {"url": url, "error": str(e)}
        with open(ALL_JSONL, "a", encoding="utf-8") as f:
            f.write(json.dumps(rec, ensure_ascii=False) + "\n")
        media = MEDIA.findall(rec.get("resp_body") or "")
        flag = "MEDIA" if media else "     "
        print(f"[{flag}] {rec['method']:4s} {rec['status']} {url[:110]}")
        for m in media[:3]:
            print("        -> " + m[:150])

    page.on("response", on_response)


def probe_api(page, course_id: str):
    """在页面上下文里发请求：Cookie 由浏览器自动带上（Token 可能在 localStorage/请求头）。"""
    out = []
    for path, name in CANDIDATES:
        url = f"https://{API_HOST}{path}"
        js = """async ([url, rid]) => {
            const r = await fetch(url, {
                method: 'POST',
                headers: {'content-type': 'application/json'},
                body: JSON.stringify({recruitAndCourseId: rid}),
                credentials: 'include',
            });
            const t = await r.text();
            return {status: r.status, body: t.slice(0, 1500)};
        }"""
        try:
            res = page.evaluate(js, [url, course_id])
        except Exception as e:
            res = {"status": None, "body": f"<evaluate failed: {e}>"}
        out.append({"name": name, "url": url, **res})
        print(f"=== POST {path} -> {res['status']}")
        print("   ", (res["body"] or "").replace("\n", " ")[:300])
    with open(os.path.join(XHR_DIR, "probe-api.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)
    return out


def harvest_tree():
    """从 xhr/all.jsonl 里找章节树（含 videoId / chapterName / name 之类）。"""
    hits = []
    with open(ALL_JSONL, encoding="utf-8") as f:
        for line in f:
            rec = json.loads(line)
            body = rec.get("resp_body") or ""
            if not body.startswith(("{", "[")):
                continue
            try:
                data = json.loads(body)
            except ValueError:
                continue
            if re.search(r"videoId|videoLessons|videoChapter|lessonId", json.dumps(data)[:200000]):
                hits.append((rec["url"], data))
    print(f"[*] 疑似章节树响应: {len(hits)}")
    best = None
    for url, data in hits:
        n = len(re.findall(r'"videoId"', json.dumps(data)))
        print(f"    {n:4d} videoId  <- {url[:100]}")
        if best is None or n > best[0]:
            best = (n, url, data)
    if not best:
        print("[!] 没找到章节树；请确认 --reload 后页面真的加载了课程目录")
        return None
    with open(os.path.join(ROOT, "chapter-tree.json"), "w", encoding="utf-8") as f:
        json.dump(best[2], f, ensure_ascii=False, indent=2)
    print(f"[+] 已保存 chapter-tree.json（来自 {best[1]}）")
    return best[2]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--cdp", default="http://127.0.0.1:9222")
    ap.add_argument("--course-id", default=None, help="默认从页面 URL 自动取")
    ap.add_argument("--reload", action="store_true", help="重载课程页以触发初始 XHR")
    ap.add_argument("--sniff", action="store_true", help="被动监听并落盘 XHR")
    ap.add_argument("--probe-api", action="store_true", help="用页面上下文直接打候选接口")
    ap.add_argument("--tree", action="store_true", help="从已抓到的 XHR 里抽章节树")
    ap.add_argument("--seconds", type=int, default=25, help="监听时长（默认25秒）")
    ap.add_argument("--enable-debugger-domain", action="store_true",
                    help="显式启用 CDP Debugger 域（★默认关闭：启用后页面里的 debugger "
                         "语句会真的暂停，正好触发智慧树反调试，会让页面卡死）")
    a = ap.parse_args()

    if a.tree:
        harvest_tree()
        return

    with sync_playwright() as p:
        try:
            browser = p.chromium.connect_over_cdp(a.cdp)
        except Exception as e:
            sys.exit(f"[!] 连不上 CDP {a.cdp}\n    {e}\n"
                     "    请先重启 Chrome：\n"
                     "    google-chrome --remote-debugging-port=9222 --remote-allow-origins=* \\\n"
                     "                  --restore-last-session &")
        ctx = browser.contexts[0]
        page = pick_page(ctx, a.course_id)
        if not page:
            print("[!] 没找到课程页。已打开的页面：")
            for pg in ctx.pages:
                print("    -", pg.url[:120])
            return
        print("[*] 选中页面:", page.url[:140])

        page_id = re.search(r"recruitAndCourseId=([0-9a-fA-F]+)", page.url)
        course_id = a.course_id or (page_id.group(1) if page_id else None)
        print("[*] recruitAndCourseId =", course_id)

        page.set_default_timeout(15000)
        # ★ 关键：默认**不**启用 Debugger 域。
        #   一旦启用，页面里的 `debugger` 语句会真的暂停，而智慧树正是靠
        #   「debugger + performance.now() 时间差」判断是否被调试 —— 上一版
        #   就是在这里把页面搞卡死的。诊断/抓包都不需要 Debugger 域。
        if a.enable_debugger_domain:
            cdp = ctx.new_cdp_session(page)
            try:
                cdp.send("Debugger.enable", {})
                cdp.send("Debugger.setSkipAllPauses", {"skip": True})
                print("[!] 已显式启用 Debugger 域（默认关闭；可能导致页面卡死）")
            except Exception as e:
                print("[!] CDP:", e)

        # 健康检查：先确认页面还能响应脚本，避免在已卡死的页面上盲跑
        try:
            state = page.evaluate(
                "() => ({ready: document.readyState, url: location.href, t: Date.now()})"
            )
            print("[*] 页面响应正常:", state["ready"], "|", state["url"][:110])
        except Exception as e:
            print("[!] 页面无响应（可能仍处在上次卡死状态）：", e)
            print("    请在浏览器里按 Ctrl+R 刷新课程页，然后重跑本命令。")
            return

        if a.sniff:
            attach_sniffer(page)
            if a.reload:
                print("[*] 重载课程页，抓初始 XHR ...")
                page.reload(wait_until="domcontentloaded")
            else:
                print("[*] 只监听，不重载。请在页面上点开一节视频。")
            page.wait_for_timeout(a.seconds * 1000)
            print(f"[*] 监听结束，明细见 {ALL_JSONL}")

        if a.probe_api:
            if not course_id:
                sys.exit("[!] 没有 recruitAndCourseId，无法探测接口")
            probe_api(page, course_id)


if __name__ == "__main__":
    main()
