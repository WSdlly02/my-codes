# Architecture v1: 课程讲义清洗、整理与索引流水线

本文记录当前已经实现并跑通的 v1 架构。它描述的是现状，不包含后续改造方案。

## 1. 目标

本项目服务于开卷考试纸质资料准备。目标不是做普通摘要，而是把课程资料整理成一套可打印、可快速定位、可跨章节检索的资料系统。

核心原则：

- 以单节课或单个课件为最小资料原子 `atom`。
- `page_md` 只作为逐页 OCR 与保守清洗缓存，不直接进入打印版。
- 最终 `atom Markdown` 是底层正文，也是打印后不可再下钻的最小查询单元。
- 章节索引、专题索引、总索引只负责导航和第一跳定位，不替代正文。
- 最终纸质定位依靠 `Lxxx` 编号和 PDF 页码映射表。

## 2. 目录结构

当前主要输出目录位于：

```text
国际航运代理理论与实务/整理与索引版本/
├── 00_总索引
├── 01_章节索引
├── 02_讲义整理
├── 03_专题索引
├── 04_检索数据
├── 99_打印版
└── 99_过程缓存
```

各目录职责：

| 目录 | 职责 |
|---|---|
| `00_总索引` | 第一跳索引，包括超短总索引、中文关键词、英文缩写、问题、单证条款流程、L 编号页码表 |
| `01_章节索引` | 以六章主干为单位生成的第二层索引 |
| `02_讲义整理` | 62 个最终 atom Markdown，打印正文的底层材料 |
| `03_专题索引` | 横向专题索引，用于跨章节、跨补充资料检索 |
| `04_检索数据` | manifest、补充卡片、统计 CSV、页码映射、benchmark 数据等机器中间数据 |
| `99_过程缓存` | PDF 渲染图、page_md、Qwen raw outputs、批量转换日志 |
| `99_打印版` | 合并后的 `final_print.md` 和最终 PDF |

## 3. Atom 类型

`manifest.jsonl` 中每条记录是一个 atom。当前共有 62 个 atom。

v1 中存在三类资料：

| 类型 | manifest 特征 | 用途 |
|---|---|---|
| 教程主干 atom | `chapter` 以 `第` 开头 | 生成章节索引和主干正文 |
| 推荐视频 atom | `chapter == "资料"` 且非单 PDF 课件 | 生成 supplement card，进入专题索引和补充正文 |
| 单 PDF 课件 atom | `atom_kind == "single_pdf_courseware"` | 生成 supplement card，作为概念总览和补充正文 |

主干教程 atom 直接进入：

```text
教程 atom -> 章节索引 -> 总索引
```

推荐视频和课件 atom 先变成卡片：

```text
补充 atom -> supplement_cards.jsonl -> 专题索引 / 总索引
```

## 4. 总体数据流

v1 主流水线：

```text
原始 docx/pdf
  -> build_manifest.py
  -> manifest.jsonl
  -> convert_atom.py / convert_all_atoms.py
  -> page images
  -> page_md
  -> atom Markdown
  -> build_chapter_indexes.py
  -> build_supplement_cards.py
  -> stats_supplement_cards.py
  -> build_topic_indexes.py
  -> build_master_indexes.py
  -> build_print_bundle.py
  -> final_print.md
  -> pandoc/xelatex -> final_print.pdf
  -> extract_pdf_page_map.py
  -> 06_L编号页码表.md / page_map.json
  -> build_print_bundle.py
  -> final_print_2.md
  -> pandoc/xelatex -> final_print_2.pdf
```

逻辑分层：

```text
第 1 层：总索引
第 2 层：章节索引 + 专题索引
第 3 层：atom Markdown 正文
过程缓存：page_md / raw_outputs / rendered images
```

## 5. manifest 构建

脚本：

```text
scripts/build_manifest.py
```

输入：

```text
国际航运代理理论与实务/
```

输出：

```text
国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl
```

职责：

- 扫描课程源目录下 `.docx` 和 `.pdf`。
- 忽略 `整理与索引版本`，避免把产物重新纳入源资料。
- 根据文件后缀识别来源角色：
  - `_原文.docx` -> `transcript`
  - `_导读.docx` -> `guide`
  - `_笔记.docx` -> `note`
  - `_PPT.pdf` -> `slides`
  - 其他单 PDF -> `courseware`
- 按章节和 base name 聚合为 atom。
- 生成稳定 `atom_id`，形如 `L001-98fc1640d4`。
- 记录源文件路径、大小、sha1、缺失角色和目标输出路径。

## 6. Atom 转换

脚本：

```text
scripts/convert_atom.py
```

单个 atom 的转换过程：

```text
source docx/pdf
  -> docx 经 LibreOffice 转 PDF
  -> PDF 经 pdftoppm 渲染为 PNG
  -> 每张 PNG 调用视觉模型做 page OCR
  -> 写入 page_md
  -> 把同一 atom 的 page_md 拼接
  -> 调用文本模型综合成最终 atom Markdown
```

默认模型：

| 阶段 | 默认模型 | 作用 |
|---|---|---|
| page OCR | `qwen3.6-35b-a3b` | 单页图片 OCR 与保守清洗 |
| atom synthesis | `qwen3.6-plus-2026-04-02` | 综合全部 page_md，生成最终 atom Markdown |

关键参数：

- page OCR：`temperature=0`，`reasoning={"effort": "low"}`
- synthesis：`temperature=0.1`，`reasoning={"effort": "medium"}`
- 默认渲染 DPI：`144`

`page_md` 的定位：

- `page_md` 是证据层缓存。
- 它用于 synthesis 阶段输入。
- 最终 atom 不能引用 `page_md`、缓存路径或原始文件路径。
- 其关系应理解为 `page_md.clone()`，不是 `page_md.as_ref()`。

最终 atom Markdown 结构由 `SYNTHESIS_PROMPT` 固定，核心小节包括：

- 本讲速查索引
- 本讲知识地图
- 核心概念与定义
- 主体关系与业务边界
- 业务范围、流程与操作规则
- 单证、条款、法律依据与责任
- 案例、异常情形与处置方法
- 易混淆点与辨析
- 高频考题与答题要点
- 关键词索引
- 与其他章节的关联

## 7. 批量转换

脚本：

```text
scripts/convert_all_atoms.py
```

职责：

- 按 manifest 调用 `convert_atom.py`。
- 支持 `--only-missing` 跳过已完成 atom。
- 支持 `--reuse-page-md` 复用已有 OCR 缓存，只重跑 synthesis。
- 支持 `--ocr-only` 只生成 page_md。
- 支持 `--parallel N` 分批并行，每批最多 N 个 atom。
- 每个 atom 的 stdout/stderr 实时写入独立 log，同时 pipe 到终端。

并行模型：

```text
manifest 未完成 atom
  -> 每批 N 个并行 convert_atom.py
  -> 本批全部成功后进入下一批
  -> 任一失败则停止
```

日志目录默认：

```text
国际航运代理理论与实务/整理与索引版本/99_过程缓存/convert_all_logs
```

## 8. OCR 上下文统计

脚本：

```text
scripts/stats_page_md.py
```

职责：

- 统计每个 atom 的 `page_md` 总字符数、非空白字符数、行数、估算 token 数。
- 标记 synthesis 输入是否有上下文风险。
- 输出 CSV 到：

```text
国际航运代理理论与实务/整理与索引版本/04_检索数据/page_md_stats.csv
```

## 9. 章节索引

脚本：

```text
scripts/build_chapter_indexes.py
```

输入：

- `manifest.jsonl`
- `02_讲义整理` 中教程主干 atom Markdown

输出：

```text
国际航运代理理论与实务/整理与索引版本/01_章节索引/*.md
```

职责：

- 只处理 `chapter` 以 `第` 开头的主干教程 atom。
- 同一章节内把多个 atom Markdown 合成为章节索引。
- 输出固定结构，包括：
  - 本章一句话
  - 本章知识地图
  - Atom 路由表
  - 核心概念索引
  - 单证、条款、主体与责任索引
  - 流程与操作索引
  - 英文缩写与术语
  - 高频问题索引
  - 易混淆点
  - 本章打印与查阅建议

章节索引是第二层索引，不复制大段 atom 正文，所有定位指向 `Lxxx`。

## 10. 补充资料卡片

脚本：

```text
scripts/build_supplement_cards.py
```

输入：

- `manifest.jsonl`
- `02_讲义整理` 中 `chapter == "资料"` 的 atom Markdown

输出：

```text
国际航运代理理论与实务/整理与索引版本/04_检索数据/supplement_cards.jsonl
```

职责：

- 只为补充资料生成机器可读 JSON 卡片。
- 推荐视频标为 `recommended_video`。
- 单 PDF 课件标为 `courseware`。
- 提取：
  - `topics`
  - `keywords_cn`
  - `keywords_en`
  - `concepts`
  - `documents`
  - `processes`
  - `legal_rules`
  - `fees_or_trade_terms`
  - `case_points`
  - `questions`
  - `best_used_for`

设计判断：

- 主干教程 atom 不强制生成 card，因为章节结构天然清楚，章节索引可直接读正文生成。
- 补充资料逻辑较松散，card 用于机器聚合和专题索引，不作为打印正文给人直接阅读。

## 11. 补充卡片统计

脚本：

```text
scripts/stats_supplement_cards.py
```

职责：

- 统计 supplement cards 中的 `topics`、`keywords_en`、`keywords_cn`。
- 帮助发现高频专题和可能遗漏的专题。

输出：

```text
04_检索数据/supplement_topics_stats.csv
04_检索数据/supplement_keywords_en_stats.csv
04_检索数据/supplement_keywords_cn_stats.csv
04_检索数据/supplement_card_stats.md
```

## 12. 专题索引

脚本：

```text
scripts/build_topic_indexes.py
```

输入：

- `01_章节索引/*.md`
- `04_检索数据/supplement_cards.jsonl`

输出：

```text
国际航运代理理论与实务/整理与索引版本/03_专题索引/*.md
```

v1 固定专题清单：

```text
航运基础
船舶与港口
货物与航线
船舶代理
货运代理
班轮运输与提单
航次租船
定期租船与光船租船
多式联运与集装箱
货运事故与索赔
海上货运公约
争议解决
费用运价与附加费
英文缩写与术语
```

专题索引生成机制：

- 主干入口来自全部章节索引。
- 补充材料来自匹配该专题的 supplement cards。
- `英文缩写与术语` 专题额外选入带 `keywords_en` 的卡片。
- 如果 card 中存在不在固定清单内的 topic，脚本会 warning。

专题索引定位方式：

- 章节名
- `atom_id`
- 补充 atom_id

专题索引是横向导航，不替代章节索引或 atom 正文。

## 13. 总索引

脚本：

```text
scripts/build_master_indexes.py
```

输入：

- `01_章节索引/*.md`
- `03_专题索引/*.md`
- `04_检索数据/supplement_cards.jsonl`

输出：

```text
国际航运代理理论与实务/整理与索引版本/00_总索引/
├── 00_超短总索引.md
├── 01_中文关键词总索引.md
├── 02_英文缩写总索引.md
├── 03_问题总索引.md
├── 04_单证条款流程索引.md
└── 05_打印顺序建议.md
```

总索引职责：

- 第一跳定位。
- 给出先翻哪里、再翻哪里。
- 不读 62 个 atom 正文。
- 不复制大段内容。
- 位置必须指向章节索引、专题索引或 `atom_id`。

## 14. 打印版合并

脚本：

```text
scripts/build_print_bundle.py
```

输入：

- `00_总索引`
- `01_章节索引`
- `02_讲义整理`
- `03_专题索引`
- `manifest.jsonl`

输出：

```text
国际航运代理理论与实务/整理与索引版本/99_打印版/final_print.md
```

打印顺序：

```text
总索引
-> 章节索引
-> 教程正文
-> 专题索引
-> 补充正文
```

实现细节：

- 用 `\newpage` 插入分页。
- 教程正文和补充正文按 `Lxxx` 排序。
- 教程正文由 manifest 中 `chapter` 以 `第` 开头的 atom 判断。
- 专题索引按固定专题顺序排列。
- 专题索引标题前自动添加 `T01` 到 `T14`。
- 在专题索引部分前插入专题目录。

PDF 生成目前通过外部命令完成，推荐路线为 Pandoc + XeLaTeX，例如：

```bash
pandoc final_print.md \
  -o final_print.pdf \
  --pdf-engine=xelatex \
  -V CJKmainfont="Noto Serif CJK SC" \
  -V geometry:margin=1.6cm \
  --toc \
  --number-sections
```

## 15. 页码映射

脚本：

```text
scripts/extract_pdf_page_map.py
```

输入：

```text
99_打印版/final_print.pdf
```

输出：

```text
00_总索引/06_L编号页码表.md
04_检索数据/page_map.json
```

职责：

- 调用 `pdftotext -layout` 提取 PDF 文本。
- 优先从 PDF 目录/页眉页尾样式文本中解析 `Lxxx -> page` 和大分区起始页。
- 若失败，则逐页搜索 `Lxxx` 和分区标题。
- 输出分区页码和 atom 起始页。

v1 采用单独的 L 编号页码表，不把页码强行注入所有索引项，避免破坏索引结构。

## 16. 本地 OCR benchmark

脚本：

```text
scripts/benchmark_ollama_ocr.py
```

目的：

- 评估本地 Ollama 视觉 OCR 模型能否替代云端 page OCR。

当前设计：

- 从 `99_过程缓存` 均匀抽取 4 个 `L0xx`。
- 每个 atom 抽取 4 张页面图片，共 16 张。
- 使用 OpenAI-compatible Ollama endpoint：

```text
http://localhost:11434/v1/
```

- 默认模型：

```text
glm-ocr:bf16
```

- 为公平对比，benchmark 已复刻 `convert_atom.py::call_qwen_page_ocr` 的 prompt、输入结构和参数：
  - `OCR_SYSTEM_PROMPT`
  - `PAGE_OCR_PROMPT`
  - `system + user(input_text + input_image)`
  - `temperature=0`
  - `reasoning={"effort": "low"}`

输出：

```text
04_检索数据/ocr_benchmark/
├── samples.json
├── *.md
├── raw_outputs/*.json
├── summary.json
└── summary.md
```

当前 benchmark 结论由人工观察得出：

- 本地小参数模型在复杂课程页面 OCR 上不稳定。
- 运行到部分样本时可能出现运算错误。
- 前若干输出质量不理想，甚至会重复输出 prompt。
- 说明小模型更适合基础 OCR，不适合 v1 中“忠实 OCR + 保守清洗 + 结构识别”的复杂 page OCR 任务。

## 17. v1 的关键假设

v1 架构依赖以下假设：

- 视觉模型能够稳定完成复杂页面 OCR、表格识别、流程图描述和保守清洗。
- `page_md` 虽然是缓存，但质量足以支撑最终 atom synthesis。
- 单个 atom 的全部 `page_md` 可以放入文本模型上下文。
- 章节索引可以直接读取主干 atom 正文，不必先生成主干 card。
- 补充资料需要 card 作为机器中间层，以便横向专题索引聚合。
- 纸质查阅主要依靠 `总索引 -> 章节/专题索引 -> L编号页码表 -> atom正文`。

## 18. v1 的已知问题

从实际运行和 benchmark 看，v1 的主要问题集中在 OCR 阶段：

- 云端视觉 OCR token 消耗极高，当前总调用量中大部分成本来自 OCR。
- 本地小参数视觉模型无法稳定替代云端复杂 OCR。
- 当前 page OCR 同时承担“文字识别、版面结构化、保守清洗、低置信度说明”，任务复杂度较高。
- OCR 失败或幻觉会污染后续 synthesis，因为 atom Markdown 依赖 page_md。
- v1 对 docx/pdf 一律走图片 OCR，对原生文本 PDF 或 docx 中可直接提取的文本没有充分利用。

这些问题是后续 v2 架构改造的主要动因。
