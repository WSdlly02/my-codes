# Linux 验证记录（2026-09-20）

最新版本已同步到 wsdlly02-pc:/home/wsdlly02/Documents/my-codes/SOPs/mooc-downloader/zhihuishu/。

**约定：仓库只放代码与文档。** 所有 `.mp4 / .psv / .csv / .json / .jsonl` 数据产物（下载文件、清单、章节树、报告、抓包明细）一律由脚本在本地生成，并被 `.gitignore` 排除；因此本文只记录结论与可复现命令，不再随仓库分发原始 JSON 证据。

**2026-09-20 追加**：新增 API 路线（`resolve.py` + `enumerate.py`）并完成真机验证，见第 7、8 项。

## 测试环境

- NixOS，Python 3.14.7。
- Playwright 1.63.0（uv 临时依赖环境）。
- 系统 Google Chrome 153.0.8010.47，无头模式，启用 Chromium sandbox。
- 系统 curl、ffmpeg/ffprobe（libx264 + aac 可用，用于现场合成测试媒体）。
- **（追加）** 已登录智慧树的 Chrome 153，以专用 `--user-data-dir` + `--remote-debugging-port=9222` 启动，用于 API 路线采集。

## 已完成检查

1. `mooc.py`、`collect.py`、`resolve.py`、`enumerate.py` 在 Linux 上通过 `py_compile`。
2. `uv run --with 'playwright==1.63.0' python -m unittest discover -s tests -v`：**8 项全部通过，0 跳过、0 失败**（1.3 秒）。
   - Chrome 集成测试验证同名课节、异步媒体地址切换、增量 JSON 保存和视频暂停；媒体样本由 ffmpeg 在临时目录现场合成，**不需要仓库内或已下载的 MP4**。
   - 7 项下载器测试验证 Referer 对照、完整下载、现有文件跳过、Range 续传、未识别部分文件保护、同大小损坏文件拒绝、截断文件拒绝及不安全编号拒绝（部分情形合并在同一测试中）。
3. 真实网站对照：第一节无 Referer 为 HTTP 403 / curl 22；带课程来源为 HTTP 206，完整大小 55,211,608 字节。（原始 JSON 落在本地 `reports/diagnosis.json`）
4. 真实课程 43 节全部访问验证通过，失败 0；每节读取 32 字节，核对 Content-Range 和 MP4 文件头，合计 1,877,385,073 字节（约 1.75 GiB）。（原始 JSON 落在本地 `reports/access.json`）
5. Linux 第 3.3 节完整下载 14,502,317 字节，MP4 结构与 ffprobe 元信息检查通过（h264 1280×720 + aac，时长 181.056s）。
6. `python3 mooc.py verify --only 3.3 --ffprobe` 复核通过，SHA-256 与 Windows 完整下载文件一致：

   09b497e018c1637848a7a6045056938625636a9cb5768e8d8d03107670fc71d7

7. **（追加）API 路线端到端跑通两轮**（已登录 Chrome 153 + 专用 profile + CDP 9222）：

   ```bash
   uv run --with 'playwright>=1.40,<2' python resolve.py --sniff --reload --tree
   python3 enumerate.py                       # 输出 reports/<recruitId>-manifest.psv
   python3 mooc.py probe --manifest reports/453243-manifest.psv
   ```

   - `videolist` 响应 18,741 字节，`Content-Type: application/x-www-form-urlencoded`，body 为 `secretStr=<签名>&dateFormate=<毫秒>`；含 9 章 / 33 主节 + 10 个 `videoSmallLessons` = **43 节**。
   - `enumerate.py` 生成的清单与当时手工逐节整理的 43 节清单对比：**43 vs 43，缺失 0，多余 0，title/duration/url 全字段不一致 0**（那份手工清单已按"数据不入库"原则移出仓库）。
   - `mooc.py probe` 对新清单：**43/43 可访问，0 失败，合计 1,877,385,073 字节**；与第 4 项历史字节数 **0 处不一致**。
   - 抽样 `1.1 / 3.3 / 9.2.2` 单独 probe 全部 accessible。
   - `initVideo` 实测**不带 Cookie、不带 Referer** 也返回 200，返回全部画质线；本课程只有「标准」非空（即 `_512`）。
   - 首轮探针失败记录（均已定位为构造错误，非接口不可用）：用 JSON body 调 `videolist` 得到 `code:-1`（应为表单编码）；空 Cookie 调 curl 得到 401。
   - 实测确认 CDP 的 `Debugger` 域会让页面假死（智慧树用 `debugger` + 时间差反调试），`resolve.py` 已默认不启用。

8. **（追加）`collect.py` 的 Chrome 集成测试改为自给自足**：
   - 旧实现要求 `downloads/ship-trade/03.03-*.mp4` 或 `ZHS_TEST_MEDIA`；`downloads/` 被忽略，样本缺失时该测试会**静默 skip**（长期看起来"通过"）。
   - 现改为由 ffmpeg 在临时目录合成 181 秒测试媒体（与 fixture 的 `00:03:01` 对齐），仓库内不再需要任何数据文件。
   - 结果：`uv run --with 'playwright==1.63.0' python -m unittest discover -s tests -v` → **Ran 8 tests, OK（8 通过 / 0 跳过）**，在没有任何 MP4 的干净工作区复现。

## 验证范围

Chrome 集成测试使用本地模拟课程页面，通过拦截请求提供现场合成的 MP4 样本，不访问真实课程，也不使用登录凭据。验证了独立 Linux 采集器的 DOM 选择、等待和保存逻辑。

未在 Linux 已登录智慧树会话中做过 `collect.py` 的真实页面逐节采集；当时那份 43 节清单来自更早的 Windows Edge 真实课程采集。换课程或站点改版时，需要按 README 第 5 节检查实际页面。

**（追加）API 路线的真实页面验证已完成**：`resolve.py --sniff --reload --tree` 在真实课程页上连跑两轮成功（reload 一次页面、被动接收响应）。其边界：

- 仅验证了一门课程；换课程需实测 `videoSmallLessons` 的有无，以及媒体是否仍为直链 MP4。
- `videolist` 的 `secretStr` 签名只能由页面 JS 生成，因此**该路线无法脱离已登录浏览器**；`initVideo` 与媒体下载则完全不需要凭据。
- 不要启用 CDP 的 `Debugger` 域（会导致页面假死）；`resolve.py` 默认关闭，需要时显式 `--enable-debugger-domain`。
- Chrome ≥136 在默认 user-data 目录下会忽略 `--remote-debugging-port`，必须显式指定 `--user-data-dir`。

MP4 结构及 ffprobe 元信息检查不等于逐帧解码。本次在 Linux 上只完整下载过第 3.3 节测试样本（14.5 MB），未再下载整套 1.75 GiB；Windows 端此前已完整下载 43 节。

## 复现命令

```bash
cd /home/wsdlly02/Documents/my-codes/SOPs/mooc-downloader/zhihuishu
python3 -m py_compile mooc.py collect.py resolve.py enumerate.py

# 单元测试 + 真浏览器集成测试（无需任何数据文件：媒体由 ffmpeg 现场合成）
uv run --with 'playwright==1.63.0' python -m unittest discover -s tests -v

# —— API 路线：需要已登录 Chrome（专用 --user-data-dir + CDP 9222）——
uv run --with 'playwright>=1.40,<2' python resolve.py --sniff --reload --tree
python3 enumerate.py                                    # -> reports/<recruitId>-manifest.psv
python3 mooc.py probe    --manifest reports/<recruitId>-manifest.psv
python3 mooc.py download --manifest reports/<recruitId>-manifest.psv --dest downloads/<课程名>

# —— DOM 路线：需要已登录 Chrome + 课程页 URL ——
uv run --with 'playwright>=1.40,<2' python collect.py \
  --cdp http://127.0.0.1:9222 --course-url '<课程播放页 URL>' --output reports/catalog.json
```

`mooc.py` 的 `--manifest` 为必填：本仓库不含清单，需先用上面任一条路线生成（默认落在 `reports/`，已被忽略）。

`downloads/`、`reports/`、`xhr/`、`evidence/`、`chrome-profile/` 以及 `*.mp4 / *.psv / *.csv / *.json / *.jsonl / *.part` 均由 `.gitignore` 排除；`chapter-tree.json` **不**入库（可由第 1 步一条命令重建）。没有创建 Git 提交或推送。
