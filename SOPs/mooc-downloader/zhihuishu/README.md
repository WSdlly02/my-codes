# 智慧树课程视频下载 SOP（Linux）

此目录把「枚举课程 → 403 对照诊断 → 批量下载 → 校验」拆成独立步骤，并保留**两条枚举路线**：
**API 路线**（`resolve.py` + `enumerate.py`，一次请求拿全量）与 **DOM 路线**（`collect.py`，逐节点击，接口改版时的保底）。
已有清单时只需 Python 3.11+ 和 curl；Playwright 仅用于枚举阶段。

## 1. 本次调查结论与适用范围

2026-09-20，上海海事大学朱墨老师的「船舶贸易」，课程目录 9 章、43 节，标示时长共 06:35:58。网页「课程资料」显示「暂无课程资料」，没有发现独立 PPT/PDF 附件。

同一 MP4：普通 curl 返回 403；补上 `Referer: https://studyvideoh5.zhihuishu.com/` 后，Range 请求返回 206，完整下载返回 200。此次资源下载不需要导出 Cookie、密码或短期签名。视频地址来自实际播放器，未猜测隐藏接口。

**（2026-09-20 追加）已定位到站点的三个真实接口**，结论是「签名只挡目录，不挡文件」：

| 环节 | 接口 | 凭据要求 |
|---|---|---|
| 章节树 | `POST https://studyservice-api.zhihuishu.com/gateway/t/v1/learning/videolist` | ⚠️ 需要 `secretStr` 签名 |
| 媒体地址 | `GET https://newbase.zhihuishu.com/video/initVideo?jsonpCallBack=result&videoID=<id>&_=<ts>` | ✅ 零凭据 |
| 媒体文件 | `https://wsvideo.zhihuishu.com/.../xxx_512.mp4` | ✅ 只需 `Referer` |

因此枚举只需借已登录页面抓一次章节树；拿到树之后，取地址与下载全程零凭据。详见第 8 节。

Windows 端已完整下载并检查 43/43 个文件，合计 **1,877,385,073 字节，约 1.75 GiB**。这里附带的是当时播放器选中的 `_512.mp4` 地址，未探查其他清晰度。Linux 验证结果见 `VALIDATION.md`。

这个流程不保证适用于其他课程、平台或未来版本。Referer 只证明本次请求来源条件的差异；403 本身不能证明开发者工具检测或 DRM。不要通过改动后缀猜测清晰度地址。

## 2. 文件说明

| 文件 | 用途 |
|---|---|
| `resolve.py` | **API 路线·枚举**：连接已登录浏览器的 CDP，抓取 `videolist` 响应并抽出章节树 → `chapter-tree.json` |
| `enumerate.py` | **API 路线·清单**：章节树 → 逐 `videoId` 调 `initVideo` → 生成 `mooc.py` 可直接使用的 `.psv` |
| `collect.py` | **DOM 路线（保底）**：Playwright 逐节读取真实媒体 URL，校验章节号、媒体就绪状态与时长，增量保存 JSON |
| `mooc.py diagnose` | 同一地址无 Referer / 有 Referer 的小范围对照请求 |
| `mooc.py probe` | 批量验证 206、Content-Range、MP4 文件头并统计大小 |
| `mooc.py download` | 按章节命名下载，失败重试、断点续传、校验、计算 SHA-256 |
| `mooc.py verify` | 核对本地文件大小和 MP4 结构，可附加 ffprobe 元信息检查 |
| `tests/test_downloader.py` | 本地 HTTP 仿真测试：Referer、Range、续传及损坏文件处理 |
| `tests/test_collector.py` | 可选真浏览器集成测试：同名课节、媒体异步切换及保存。媒体样本由 ffmpeg 现场合成 |
| `VALIDATION.md` | Linux 实机测试结论（原始证据不随仓库分发） |

**本仓库只放代码与文档。** `.mp4 / .psv / .csv / .json / .jsonl` 等数据产物一律由脚本在本地生成，并被 `.gitignore` 排除（`downloads/`、`reports/`、`xhr/`、`evidence/`）。清单、章节树、报告、下载文件都不入库。

## 3. 快速开始：先有清单，再下载

本仓库不含任何课程数据，清单必须先由**第 5 节（DOM 路线）**或**第 8 节（API 路线）**生成。
清单到手后，下载与校验完全不需要浏览器、不需要任何凭据。

```bash
cd /home/wsdlly02/Documents/my-codes/SOPs/mooc-downloader/zhihuishu
python3 --version
curl --version

# 清单（enumerate.py 的默认输出位置；也可用 --manifest 指向别处）
MANIFEST=reports/<recruitId>-manifest.psv

# 单节对照诊断：无 Referer / 有 Referer（默认取清单第一节）
python3 mooc.py diagnose --manifest "$MANIFEST" --report reports/diagnosis.json

# 全部地址小范围检查：每个视频只读取 32 字节
python3 mooc.py probe --manifest "$MANIFEST"

# 先完整下载一节验证（例如体积较小的 3.3）
python3 mooc.py download --manifest "$MANIFEST" --only 3.3 --ffprobe

# 下载其余全部；相同大小且 MP4 结构有效的现有文件会跳过
python3 mooc.py download --manifest "$MANIFEST" --dest downloads/<课程名>

# 再核对；ffprobe 需要系统已有 ffmpeg 工具包
python3 mooc.py verify --manifest "$MANIFEST" --dest downloads/<课程名> --ffprobe
```

`--dest /你的路径` 改变目标目录（默认 `downloads/`）；`--only 1.1,3.3` 选择课节。

所有模式都会留下 JSON 报告（与清单同目录，默认落在忽略规则内）。`download-results.json` 记录 SHA-256、服务器大小和成功/失败状态；失败返回非零退出码。`verify` 会重新访问服务器核对大小，因此需要网络。

Ctrl+C 中断后重新执行同一命令即可续传。中间文件为 `.mp4.part`，旁边 `.mp4.part.json` 记录 URL、大小和 ETag；标识不一致时拒绝接着写，保留现场。不要同时向同一目标目录运行多个下载进程。已有同名文件损坏或大小不符时不会覆盖。

MP4 检查验证顶层 atom 边界及 ftyp/moov/mdat，不能替代逐帧解码。`--ffprobe` 也只是元信息检查。需要更强检查时可另行使用 `ffmpeg -v error -i 文件.mp4 -f null -`。

## 4. Linux 依赖与 NixOS

下载器仅使用 Python 标准库调用 curl，不要求 pip、jq 或 PowerShell。当前 wsdlly02-pc 已有 Python、curl 和 ffprobe。

NixOS 缺依赖时可在临时环境中运行（不会修改系统配置）：

```bash
nix shell nixpkgs#python3 nixpkgs#curl nixpkgs#ffmpeg
```

只需下载本课程时，直接跳到第3节；不用启动浏览器或迁移 Windows 登录态。

## 5. 重新采集另一门课：DOM 路线（Playwright）

> **更省事的做法见第 8 节（API 路线，一次请求拿全量）。** 本节是 DOM 路线，作为接口不可用时的保底。

本机浏览器必须能正常打开你有权访问的课程播放页。`collect.py` 只读取目录及播放器 DOM，不打开开发者控制台，不改写学习进度、不做测试题、不处理验证码；逐节打开可能触发平台正常的访问记录。遇到实际可见验证码需手动处理。

安装采集依赖（二选一）：

```bash
# 已有uv的机器：执行时临时解析Playwright依赖
uv run --with 'playwright>=1.40,<2' python collect.py --help

# 或隔离虚拟环境
python3 -m venv .venv
.venv/bin/python -m pip install -r requirements-browser.txt
```

使用系统 Chromium/Chrome 的 CDP 连接，不必安装 Playwright 自带浏览器。在 NixOS 上优先使用现有 Google Chrome，避免下载版 Chromium 的动态链接依赖问题。

在 Linux 图形桌面终端手动启动一个专用浏览器配置目录并登录课程：

```bash
google-chrome \
  --remote-debugging-address=127.0.0.1 \
  --remote-debugging-port=9222 \
  --user-data-dir="$HOME/.local/share/zhihuishu-browser" \
  'https://onlineweb.zhihuishu.com/onlinestuh5'
```

调试端口能控制该浏览器会话，只绑定本机回环地址，不对局域网或公网开放。独立配置目录需要自行登录一次；不要复制正在运行的日常浏览器配置。普通浏览器如果启动时未启用 CDP，无法仅靠这个脚本直接附加。连接 CDP 与打开 DevTools 窗口不同，但仍不能保证平台不检测自动化。

> **`--user-data-dir` 不是可选项**：Chrome ≥136 会忽略默认 user-data 目录下的 `--remote-debugging-port`（防止本机程序借调试端口读 cookie）。不给显式目录，9222 永远不会监听。

在专用浏览器中进入课程播放页，复制完整 URL，再执行：

```bash
uv run --with 'playwright>=1.40,<2' python collect.py \
  --cdp http://127.0.0.1:9222 \
  --course-url '这里粘贴浏览器当前课程播放页的完整URL' \
  --output catalog.json

python3 mooc.py diagnose --manifest catalog.json
python3 mooc.py probe --manifest catalog.json
python3 mooc.py download --manifest catalog.json --dest downloads/new-course
```

必须有且只有一个 URL 完全匹配的已打开课程页。采集逐节等待 `current_play`、新 `currentSrc` 和媒体元信息，并用目录时长交叉核对。每一节都会增量写入结果；异常时保留已采集内容。恢复采集请使用新的输出文件名，避免覆盖原清单。相邻课节共用同一 URL、blob/HLS 或页面选择器变化时会停止，应重新检查页面。

采集器不会关闭你的浏览器；结束后停留在最后读取的课节并暂停视频。它使用本次页面观察到的 `li.video`、`.hour`、`#vjs_container_html5_api` 等选择器，网站改版后需更新。此脚本不是通用智慧树接口爬虫。

## 6. 最小curl命令与故障定位

```bash
curl --fail --retry 3 --continue-at - \
  --referer 'https://studyvideoh5.zhihuishu.com/' \
  --output '课节.mp4.part' \
  '从播放器读取的真实MP4地址'
```

- `403`：先执行 diagnose 比较 Referer；两种请求都失败就回到已登录页面核对当前资源，不批量重复猜测。
- `000`、curl 7/28：连接或超时问题，不是已确认的403。检查 DNS、代理和当前执行环境网络限制。
- `206` + 正确 Content-Range：分段读取成功；仍需完整下载及校验，不能当作完整文件已保存。
- 浏览器能播放但跨域脚本 fetch 失败：可能是 CORS；与命令行请求的403应分开调查。
- cookie/短期签名/HLS/DRM：本工具未实现，不能据此方案宣称已支持。
- **页面假死、拒绝响应**：检查是否启用了 CDP 的 `Debugger` 域（见第 8.3 节）。启用后页面里的 `debugger` 语句会真的暂停，触发平台的反调试判断。

## 7. 本地自动测试

```bash
python3 -m unittest discover -s tests -v
```

测试启动仅绑定 127.0.0.1 的临时 HTTP 服务，数据存放在临时目录，不访问真实课程。真实网站验证单独记录于 `VALIDATION.md`。

需要同时运行浏览器测试时，直接执行即可——媒体样本由 ffmpeg 在临时目录现场合成，不需要先下载任何视频：

```bash
uv run --with 'playwright==1.63.0' python -m unittest discover -s tests -v
```

已在 Linux 上验证 8 项全部通过。浏览器测试使用系统 Google Chrome 与本地模拟页面；缺 Playwright、Chrome 或 ffmpeg 时该项会跳过（也可用 `ZHS_TEST_MEDIA` 指定现成样本）。

参考官方文档：[Playwright connect_over_cdp](https://playwright.dev/python/docs/api/class-browsertype#browser-type-connect-over-cdp)、[等待页面条件](https://playwright.dev/python/docs/api/class-page#page-wait-for-function)。

## 8. 自动枚举课程：API 路线（2026-09-20 新增）

### 8.1 协议只有三个接口

| 环节 | 接口 | 凭据要求 |
|---|---|---|
| 章节树 | `POST https://studyservice-api.zhihuishu.com/gateway/t/v1/learning/videolist`<br>`Content-Type: application/x-www-form-urlencoded`<br>body：`secretStr=<签名>&dateFormate=<毫秒时间戳>` | ⚠️ 需要 `secretStr` 签名，只能由页面 JS（`crypto.js`）生成 |
| 媒体地址 | `GET https://newbase.zhihuishu.com/video/initVideo?jsonpCallBack=result&videoID=<id>&_=<ts>` | ✅ 零凭据（实测不带 Cookie、不带 Referer 也返回 200） |
| 媒体文件 | `https://wsvideo.zhihuishu.com/.../*_512.mp4` | ✅ 只需 `Referer: https://studyvideoh5.zhihuishu.com/` |

要点：

- **签名只挡目录，不挡文件。** 所以章节树只需抓一次；拿到后取地址与下载全程零凭据。
- `videolist` 返回 `videoChapterDtos[] → videoLessons[] → videoSmallLessons[]`，**三级层级**（课节号形如 `8.1.1` 来自 `videoSmallLessons`）。本课程为 9 章 / 33 主节 + 10 子节 = 43 节。
- `initVideo` 返回全部画质线（`lines[]`：`lineName` 标准/流畅…），本课程只有「标准」非空（即 `_512`）。
- `chapter-tree.json` 仅 ≈19KB，**由脚本本地生成、不入库**（`.gitignore` 排除）。它是唯一需要凭据才能获得的产物；只要文件还在本地，取地址与下载都不需要登录，重建只需 8.2 第 1 步那一条命令。

### 8.2 三步流水线

```bash
cd /home/wsdlly02/Documents/my-codes/SOPs/mooc-downloader/zhihuishu

# 1) 从已登录浏览器抓章节树（会 reload 一次课程页）
uv run --with 'playwright>=1.40,<2' python resolve.py --sniff --reload --tree

# 2) 章节树 -> mooc.py 可用的清单（默认输出 reports/<recruitId>-manifest.psv）
python3 enumerate.py
#   或显式指定： python3 enumerate.py --tree chapter-tree.json --out /路径/x.psv

# 3) 探活 + 下载（零凭据，只需 Referer）
MANIFEST=reports/<recruitId>-manifest.psv
python3 mooc.py probe    --manifest "$MANIFEST"
python3 mooc.py download --manifest "$MANIFEST" --dest downloads/<课程名>
```

`resolve.py` 的其他用法：

```bash
uv run --with 'playwright>=1.40,<2' python resolve.py                  # 只挂上去，确认页面与 recruitAndCourseId
uv run --with 'playwright>=1.40,<2' python resolve.py --sniff --seconds 25   # 只监听，不 reload（手动点一节）
uv run --with 'playwright>=1.40,<2' python resolve.py --probe-api      # 用页面上下文试探接口可用性
uv run --with 'playwright>=1.40,<2' python resolve.py --tree           # 从已抓到的 XHR 里抽章节树
```

XHR 明细落在 `xhr/all.jsonl`（请求头、请求体、响应体），便于排查签名/参数。

### 8.3 两个必须知道的坑

1. **Chrome ≥136 会忽略默认 profile 下的 `--remote-debugging-port`。** 必须显式指定 `--user-data-dir`，否则 `curl http://127.0.0.1:9222/json/version` 永远连不上（进程参数里能看到 flag，端口却不监听）：

   ```bash
   google-chrome --user-data-dir="$HOME/.local/share/zhs-cdp" \
     --remote-debugging-port=9222 --remote-allow-origins=* \
     'https://studyvideoh5.zhihuishu.com/stuStudy?recruitAndCourseId=<id>'
   ```

   `--remote-allow-origins=*` 是 Chrome 111+ 连接 CDP 的必要条件。

2. **不要启用 CDP 的 `Debugger` 域。** 启用后页面里的 `debugger` 语句会真的暂停，而智慧树用「`debugger` + `performance.now()` 时间差」判断是否被调试 → 页面假死、拒绝响应。`resolve.py` 默认**不**启用（需要时用 `--enable-debugger-domain` 显式打开）。脚本会先做一次响应性健康检查，若页面已卡死会提示按 Ctrl+R 恢复。

### 8.4 与 DOM 路线（第 5 节）的关系

两条路都产出 `mooc.py` 能吃的清单，可共存，按需选择：

| | API 路线（本节） | DOM 路线（第 5 节，`collect.py`） |
|---|---|---|
| 枚举成本 | 1 次 `videolist` + N 次 `initVideo`（可并发，秒级） | N 次点击 + N 次等待（线性） |
| 平台副作用 | reload 一次页面 | 逐节打开，留下访问记录 |
| 信息量 | 全画质线 + `videoId` + 时长 + 三级层级 | 仅播放器当前选中的那条线 |
| 失效方式 | 签名/接口改版会**明确报错** | DOM 改版会**静默**失效 |
| 依赖未公开接口 | 是 | 否（纯 DOM）→ **保底路线** |

当 API 路线走不通（未登录、签名变更、接口下线）时，回退 `collect.py`。两者都经测试：
`collect.py` 有 Chrome 集成测试（见 `VALIDATION.md` 第 8 项）。
