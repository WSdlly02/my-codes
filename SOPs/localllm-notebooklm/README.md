# localllm-notebooklm

为下一代学习系统保留的小型工具集。计划包括学期中交互学习、考前索引、预测卷与开卷资料构建；这些新流程尚未实现。

## 保留的工具

| 入口 | 用途 |
| --- | --- |
| `src/bin/fix-md.rs` | 修复 OCR Markdown 的常见公式格式及 HTML 表格 |
| `src/bin/batch-paddle-ocr.rs` | 按 PDF 清单批量调用 PaddleOCR，保存文字和图片 |
| `scripts-v2/normalize_to_pdf.py` | 使用 LibreOffice 将课程文档归一化为 PDF |
| `scripts-v2/build_pdf_manifest.py` | 从归一化记录生成 OCR 输入清单 |
| `scripts-v2/render_markdown_pdf.py` | 使用 Pandoc 和 XeLaTeX／Chrome 将 Markdown 渲染为 A4 PDF |
| `scripts-v2/build_pdf_nup.py` | PDF 合并及多页拼版 |

保留现有入口路径，参数可通过各工具的 `--help` 查看。Python 依赖配置沿用旧环境，尚未精简；课程资料和本地环境不纳入 Git。

## batch-paddle-ocr

一本教材一份 TOML 配置，示例见 `configs/liner-shipping.toml`（沿用
`demo-for-liner-shipping.py` 的参数）。`model` 和 `[optionalPayload]` 必填；
参数保持 API 原名并直接发送，不做配置继承或命令行覆盖。
未知的顶层字段会报错；`optionalPayload` 的字段由远端 API 解释，不在本地维护参数白名单。
密钥仍通过环境变量或 `.env` 中的 `PP_API_KEY` 提供，不写入配置。

清单采用 JSONL，每行一个 PDF，例如从仓库根目录运行时：

```json
{"id":"textbook","pdf_name":"班轮运输理论与实务","pdf_path":"班轮运输理论与实务/班轮运输理论与实务.pdf"}
```

将清单保存为 `班轮运输理论与实务/manifest.jsonl`，然后执行：

```bash
cargo run --release --bin batch-paddle-ocr -- \
  --config configs/liner-shipping.toml \
  --manifest '班轮运输理论与实务/manifest.jsonl' \
  --cache-dir '班轮运输理论与实务/ocr-cache'
```

`--concurrency`、`--poll-interval`、`--retry` 仍是运行参数，默认分别为 4、5 秒、2 次额外重试。
一份清单中的 PDF 共用指定配置，不同配置分开运行。相对 PDF 路径以运行目录为准，
`id` 应使用唯一的简单目录名。

结果保存在 `<cache-dir>/<id>/`，包含 `merged.md`、`pages/`、`images/`、
`result.jsonl` 和实际提交的模型及参数快照 `ocr-config.json`。
只有非空 `merged.md` 且快照与当前配置一致才跳过；批次开始前统一检查，
配置变化或旧缓存缺少快照时直接报错，请换缓存目录或清单 id 后重新运行。
比较的是解析后的参数值，注释和字段顺序不影响缓存复用。
此检查不校验 PDF 内容变化；更换输入文件也应使用新目录或 id。
快照记录客户端请求，不能保证远端模型更新后输出逐字一致。

示例包含脚注和旁注过滤，使用前应按教材内容决定是否保留。
参数沿用 demo 不代表已验证所有开关在托管 API 上的实际效果，尤其是多页重构与跨页表格合并。
失败重试仍会重新提交整个任务。

## fix-md

```bash
cargo run --bin fix-md -- 'input.md'
cargo run --bin fix-md -- 'input.md' -o 'output.md'
```

默认在原文件旁生成 `input_fixed.md`。已有输出需要用 `--force` 才允许替换。公式与表格的自动修复结果仍需检查，尤其是合并单元格等复杂表格。

## 本地归档

旧课程生成脚本、讨论与工作流文档、根目录旧编译产物已迁至 `archive/2026-09-24-legacy/`，内部保留原相对路径。归档仅供提取代码或思想，旧脚本的输入路径和运行环境可能已失效。

`archive-manifest.json` 记录原路径、大小和 SHA-256。归档目录已被 Git 忽略，仅保存在本机，不会随 Git 提交或克隆传递；旧课程资料仍在原位置。
