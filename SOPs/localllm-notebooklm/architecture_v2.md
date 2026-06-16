# Workflow v2: PDF OCR Cache

本文档描述当前 v2 工作流的第一阶段：把课程资料中的 PDF 批量转换为可复用的 Markdown OCR 缓存。

这一阶段只解决一个问题：

```text
PDF -> PaddleOCR -> merged.md + pages/ + images/
```

它不理解课程、章节、atom、专题，也不负责最终讲义综合。后续的 atom 合成、章节索引、专题索引应建立在本阶段产出的 OCR Markdown 之上。

## 设计原则

1. OCR 阶段保持 PDF 级别

   每一条 manifest 记录只表示一个 PDF 文件。程序只知道 `id`、`pdf_name`、`pdf_path`，不引入 role、chapter、lesson、atom 等课程概念。

2. manifest 只读

   `manifest.jsonl` 是输入清单，不写入运行状态。运行状态由文件系统上的最终产物判断。

3. 完成状态极简

   `<cache-dir>/<id>/merged.md` 存在且非空，即认为该 PDF 已完成 OCR。再次运行时自动跳过。

4. 错误即从头重来

   每个 PDF task 的中间文件写入 `<cache-dir>/<id>/.tmp/`。如果发生错误，程序清理 `.tmp/`，下一次重试从该 PDF 的起点重新开始。

5. 成功后再提交产物

   只有当 PaddleOCR job 完成、结果 JSONL 下载完成、页面 Markdown 和图片全部写入成功后，才把 `.tmp/` 中的产物移动到正式目录。

## 当前程序

Rust 入口：

```text
src/bin/batch-paddle-ocr.rs
```

共享结构与辅助函数：

```text
src/lib.rs
```

PaddleOCR 模型：

```text
PaddleOCR-VL-1.6
```

API key 从环境变量读取：

```text
PP_API_KEY
```

可以放在 `.env` 中。

## 目录规范

推荐每个课程使用如下结构：

```text
<课程名>/
├── manifest.jsonl
├── 原始资料...
└── 整理与索引版本/
    └── 99_过程缓存/
        └── <pdf_id>/
            ├── merged.md
            ├── result.jsonl
            ├── pages/
            │   ├── page_0001.md
            │   ├── page_0002.md
            │   └── ...
            ├── images/
            │   └── ...
            └── error.log
```

说明：

- `manifest.jsonl` 放在课程根目录。
- `99_过程缓存/<pdf_id>/` 是每个 PDF 的 OCR 缓存目录。
- `merged.md` 是后续 LLM 综合阶段最应该读取的文件。
- `pages/` 是逐页缓存，方便人工排查 OCR 错误。
- `images/` 保存 PaddleOCR 从 Markdown 或 layout result 中提取出的图片。
- `result.jsonl` 保存 PaddleOCR 原始结果，便于以后重新解析。
- `error.log` 只记录最近一次失败原因。

## Manifest 格式

每行一个 JSON object，字段固定为：

```json
{"id":"P000001-a1b2c3d4","pdf_name":"航运代理基本概念_第一讲_导读","pdf_path":"国际航运代理理论与实务/第一章 国际航运代理概论/航运代理基本概念_第一讲_导读.pdf"}
```

字段含义：

```text
id        PDF 的稳定标识，对应 <cache-dir>/<id>/
pdf_name  生成 Markdown 时使用的人类可读标题
pdf_path  PDF 文件路径
```

命名建议：

```text
P000001-<hash>
P000002-<hash>
...
```

`id` 不应该包含课程语义。课程、章节、atom 分组应该放到后续阶段的独立 manifest 中。

## 使用流程

### 1. 预处理为 PDF

如果原始资料是 DOCX、PPTX 或其他格式，先统一转换为 PDF。

v2 OCR 阶段默认输入已经是 PDF，不负责办公文档转换。

### 2. 构造 manifest.jsonl

在课程根目录创建：

```text
<课程名>/manifest.jsonl
```

每个 PDF 一行：

```json
{"id":"P000001-98fc1640","pdf_name":"文件名A","pdf_path":"<课程名>/资料/文件名A.pdf"}
{"id":"P000002-a31d90ef","pdf_name":"文件名B","pdf_path":"<课程名>/资料/文件名B.pdf"}
```

### 3. 设置 PaddleOCR API key

方式一：当前 shell 设置。

```bash
export PP_API_KEY='...'
```

方式二：写入项目根目录 `.env`。

```text
PP_API_KEY=...
```

### 4. 运行批量 OCR

```bash
cargo run --bin batch-paddle-ocr -- \
  --manifest <课程名>/manifest.jsonl \
  --cache-dir <课程名>/整理与索引版本/99_过程缓存 \
  --concurrency 4 \
  --poll-interval 5 \
  --retry 2
```

参数说明：

```text
--manifest       PDF manifest 路径
--cache-dir      OCR 缓存根目录
--concurrency    并发处理的 PDF 数量，默认 4
--poll-interval  查询 PaddleOCR job 状态的间隔秒数，默认 5
--retry          单个 PDF task 失败后的重试次数，默认 2
```

### 5. 查看输出

每个 PDF 成功后会生成：

```text
<cache-dir>/<id>/merged.md
<cache-dir>/<id>/pages/page_0001.md
<cache-dir>/<id>/result.jsonl
<cache-dir>/<id>/images/
```

`merged.md` 的结构：

```markdown
---
id: P000001-98fc1640
pdf_name: 文件名A
pdf_path: <课程名>/资料/文件名A.pdf
ocr_provider: PaddleOCR-VL-1.6
---

# 文件名A

<!-- PAGE 0001 -->

第一页 OCR Markdown

<!-- PAGE 0002 -->

第二页 OCR Markdown
```

## 断点续跑与失败处理

完成判定：

```text
<cache-dir>/<id>/merged.md 存在且非空
```

如果满足完成判定，再次运行会输出：

```text
[skip] <id> <pdf_name>
```

如果某个 PDF 失败：

1. 程序清理 `<cache-dir>/<id>/.tmp/`
2. 写入 `<cache-dir>/<id>/error.log`
3. 如果还有 retry 次数，则重新提交该 PDF 的 PaddleOCR job
4. 如果 retry 用尽，则该 task 记为 failed

失败不会修改已完成的 `merged.md`。

如果需要强制重跑某个 PDF，删除对应目录中的 `merged.md` 即可：

```text
<cache-dir>/<id>/merged.md
```

下次运行时该 PDF 会重新处理。

## 与后续阶段的关系

本阶段产出的是 OCR 缓存，不是最终讲义。

后续建议拆成独立阶段：

```text
PDF OCR cache
  -> atom synthesis manifest
  -> atom markdown
  -> chapter indexes
  -> topic indexes
  -> master indexes
  -> print bundle
```

其中 atom synthesis manifest 可以引用多个 PDF OCR 结果：

```json
{"atom_id":"L001","title":"航运代理基本概念_第一讲","pdf_ids":["P000001-98fc1640","P000002-a31d90ef","P000003-b702e91a"]}
```

这一步才引入课程语义，例如章节、课型、资料来源、推荐视频、课件等。

## 当前边界

本阶段不做：

- DOCX/PPTX 到 PDF 的转换
- PDF 分组为 atom
- 章节识别
- 课程专用排序
- 术语抽取
- LLM 总结
- 打印版排版

本阶段只做：

- 读取 PDF manifest
- 并发提交 PaddleOCR job
- 轮询 job 状态
- 下载 PaddleOCR JSONL
- 提取每页 Markdown
- 下载 OCR 结果中的图片
- 合并为 `merged.md`
- 提供可断点续跑的文件系统缓存

