#!/usr/bin/env python3
"""【方式二】直接给学号查个人课表 —— 不访问"课表入口页"

和方式一（tests/fetch_student_table.py，也就是二进制 export-schedule 的做法）对比：

  方式一（二进制）:
      GET  课表入口页 courseTableForStd.action
            └─ 从页面 JS 里正则抠出 == "std" 分支的学号（416704）
      POST courseTableForStd!courseTable.action   setting.kind=std & ids=<抠出来的学号>
            ↑ 学号是"从页面里发现的"，用户不用告诉程序

  方式二（本脚本）:
      （跳过入口页）
      POST courseTableForStd!courseTable.action   setting.kind=std & ids=<你输入的学号>
            ↑ 学号直接由命令行给

也就是说：**POST 那一步完全一样，只差"学号从哪来" + 有没有前置的入口页请求**。

用法: python3 tests/fetch_student_table_by_id.py <学号> [学期ID]
例如: python3 tests/fetch_student_table_by_id.py 416704 415
"""
import json
import re
import sys
import urllib.parse
import urllib.request

if len(sys.argv) < 2:
    raise SystemExit(__doc__)

STUDENT_ID = sys.argv[1]
SEMESTER = sys.argv[2] if len(sys.argv) > 2 else "415"

BASE = "https://jwxt.shmtu.edu.cn/shmtu"
UA = "Mozilla/5.0 (X11; Linux x86_64; rv:154.0) Gecko/20100101 Firefox/154.0"

cookies = "; ".join(
    f"{c['name']}={c['value']}"
    for c in json.load(open("cache/cookies.json"))["cookies"]
)


def request(url, data=None, headers=None):
    h = {"Cookie": f"semester.id={SEMESTER}; {cookies}",
         "User-Agent": UA, "Accept-Language": "en,zh-CN;q=0.9,zh;q=0.8"}
    h.update(headers or {})
    body = urllib.parse.urlencode(data).encode() if data is not None else None
    req = urllib.request.Request(url, data=body, headers=h)
    try:
        resp = urllib.request.urlopen(req, timeout=60)
    except urllib.error.HTTPError as e:
        raise SystemExit(f"请求失败 {e.code}: {e.headers.get('Location')}")
    return resp.read().decode("utf-8", "replace")


# ---- 就这一步：直接 POST，没有前面的入口页请求 ----
print(f"[方式二] 学期={SEMESTER}  直接使用你输入的学号 = {STUDENT_ID}")
html = request(
    f"{BASE}/courseTableForStd!courseTable.action",
    data={"ignoreHead": "1", "setting.kind": "std", "startWeek": "1",
          "semester.id": SEMESTER, "ids": STUDENT_ID},
    headers={"Accept": "*/*", "Content-Type": "application/x-www-form-urlencoded",
             "Origin": "https://jwxt.shmtu.edu.cn",
             "Referer": f"{BASE}/courseTableForStd.action",
             "X-Requested-With": "XMLHttpRequest"},
)
out = f"student-table-{SEMESTER}-{STUDENT_ID}.html"
open(out, "w", encoding="utf-8").write(html)
print(f"原始页面：  {html}")
print(f"  返回 {len(html)} 字节，已存成 {out}")


# ---- 解析：和方式一同一套 ----
ACT = re.compile(r'new TaskActivity\("([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)","([^"]*)"\)')


def week_numbers(state):
    return [i + 1 for i, c in enumerate(state) if c == "1"]


def render(ws):
    ws = sorted(ws)
    if not ws:
        return "-"
    out, start, prev = [], ws[0], ws[0]
    for w in ws[1:]:
        if w == prev + 1:
            prev = w
            continue
        out.append(f"{start}-{prev}" if start != prev else f"{start}")
        start = prev = w
    out.append(f"{start}-{prev}" if start != prev else f"{start}")
    return "、".join(out) + "周"


seen = {}
for slot, teacher, code, name, _, room, state in ACT.findall(html):
    seen.setdefault(name, {"teachers": set(), "rooms": set(), "weeks": set()})
    seen[name]["teachers"].add(teacher)
    seen[name]["rooms"].add(room)
    seen[name]["weeks"].update(week_numbers(state))

print(f"  解析到 {len(seen)} 门课：")
for name in sorted(seen):
    v = seen[name]
    print(f"    - {name}")
    print(f"        教师: {'、'.join(sorted(v['teachers']))}   教室: {'、'.join(sorted(v['rooms']))}")
    print(f"        周次: {render(v['weeks'])}")
