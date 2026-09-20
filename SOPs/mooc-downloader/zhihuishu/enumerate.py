#!/usr/bin/env python3
"""章节树 -> mooc.py 可用的清单（.psv）

输入：chapter-tree.json（即 /gateway/t/v1/learning/videolist 的响应）
流程：遍历 videoChapterDtos -> videoLessons -> videoSmallLessons，
      对每个 videoId 调 newbase 的 initVideo（实测**无需任何凭据**）拿媒体地址，
      输出 number|title|duration|videoId|url 供 mooc.py 使用。

用法：
    python3 enumerate.py                                   # 用默认 chapter-tree.json
    python3 enumerate.py --tree chapter-tree.json --out reports/my-course.psv
    python3 enumerate.py --workers 8                       # 并发取地址
"""
from __future__ import annotations

import argparse
import json
import re
import sys
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

ROOT = Path(__file__).resolve().parent
INIT_VIDEO = "https://newbase.zhihuishu.com/video/initVideo?jsonpCallBack=result&videoID={vid}&_={ts}"
UA = ("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/153.0.0.0 Safari/537.36")


def flatten(tree: dict) -> list[dict]:
    """把章节树摊平成 [(number, title, video_id, seconds), ...]，保留三级编号。"""
    data = tree.get("data", tree)
    out = []
    for ch in data.get("videoChapterDtos") or []:
        cnum = ch.get("orderNumber")
        for les in ch.get("videoLessons") or []:
            lnum = les.get("orderNumber")
            smalls = les.get("videoSmallLessons") or []
            if smalls:
                for sub in smalls:
                    out.append({
                        "number": f"{cnum}.{lnum}.{sub.get('orderNumber')}",
                        "title": (sub.get("name") or "").strip(),
                        "video_id": sub.get("videoId"),
                        "sec": sub.get("videoSec"),
                        "chapter": (ch.get("name") or "").strip(),
                    })
            else:
                out.append({
                    "number": f"{cnum}.{lnum}",
                    "title": (les.get("name") or "").strip(),
                    "video_id": les.get("videoId"),
                    "sec": les.get("videoSec"),
                    "chapter": (ch.get("name") or "").strip(),
                })
    return out


def hms(sec) -> str:
    try:
        sec = int(sec or 0)
    except (TypeError, ValueError):
        return ""
    return f"{sec // 3600:02d}:{sec % 3600 // 60:02d}:{sec % 60:02d}"


def init_video(video_id) -> tuple[str, list[dict]]:
    """返回 (最佳媒体地址, 全部画质线)。实测不需要 Cookie / Referer。"""
    req = urllib.request.Request(
        INIT_VIDEO.format(vid=video_id, ts=1),
        headers={"User-Agent": UA, "Referer": "https://studyvideoh5.zhihuishu.com/"},
    )
    with urllib.request.urlopen(req, timeout=25) as resp:
        body = resp.read().decode("utf-8", "replace")
    m = re.search(r"result\((.*)\)\s*$", body, re.S)
    payload = json.loads(m.group(1) if m else body)
    lines = ((payload or {}).get("result") or {}).get("lines") or []
    best = ""
    for line in lines:                        # 优先 lineDefault，其次第一条非空
        if line.get("lineUrl") and line.get("lineDefault"):
            best = line["lineUrl"]
            break
    if not best:
        for line in lines:
            if line.get("lineUrl"):
                best = line["lineUrl"]
                break
    return best, lines


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tree", type=Path, default=ROOT / "chapter-tree.json")
    ap.add_argument("--out", type=Path, default=None)
    ap.add_argument("--workers", type=int, default=6)
    a = ap.parse_args()

    tree = json.loads(a.tree.read_text(encoding="utf-8"))
    data = tree.get("data", tree)
    recruit = data.get("recruitId") or "course"
    out = a.out or ROOT / "reports" / f"{recruit}-manifest.psv"

    items = flatten(tree)
    if not items:
        sys.exit("[!] 章节树里没解析出课节")
    print(f"[*] 解析到 {len(items)} 节（含 videoSmallLessons 三级编号）")

    def job(it):
        try:
            url, lines = init_video(it["video_id"])
            it["url"] = url
            it["lines"] = lines
        except Exception as e:
            it["url"] = ""
            it["error"] = str(e)
        return it

    with ThreadPoolExecutor(max_workers=a.workers) as pool:
        items = list(pool.map(job, items))

    ok = [i for i in items if i["url"]]
    bad = [i for i in items if not i["url"]]
    print(f"[*] 取到媒体地址 {len(ok)}/{len(items)}；失败 {len(bad)}")
    for i in bad:
        print(f"    [!] {i['number']} {i['title']} videoId={i['video_id']} {i.get('error','')}")
    quals = {}
    for i in ok:
        for line in i.get("lines") or []:
            if line.get("lineUrl"):
                quals.setdefault(line.get("lineName"), 0)
                quals[line.get("lineName")] += 1
    print("[*] 可用画质线:", quals)

    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", encoding="utf-8", newline="") as f:
        f.write("number|title|duration|videoId|url\n")
        for i in items:
            f.write(f"{i['number']}|{i['title']}|{hms(i['sec'])}|{i['video_id']}|{i['url']}\n")
    print(f"[+] 已写出清单: {out}")
    try:
        shown = out.relative_to(ROOT)
    except ValueError:
        shown = out
    print(f"    下一步: python3 mooc.py probe    --manifest {shown}")
    print(f"            python3 mooc.py download --manifest {shown} --dest downloads/<课程名>")
    return 0 if not bad else 1


if __name__ == "__main__":
    sys.exit(main())
