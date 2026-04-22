#!/usr/bin/env python3
"""Build top-level indexes from chapter and topic indexes."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path

from dotenv import load_dotenv
from openai import OpenAI


DEFAULT_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"
DEFAULT_MODEL = "qwen3.6-plus-2026-04-02"

SYSTEM_PROMPT = """你是国际航运代理开卷考试总索引编制助手。
目标是把章节索引、专题索引和补充资料卡片整合为顶层索引。
总索引用于第一跳定位，不替代章节索引、专题索引或 atom 正文。只依据输入材料，不编造。输出中文 Markdown。"""

MASTER_INDEXES = {
    "00_超短总索引.md": """生成“超短总索引”。

结构：
# 超短总索引

## 1. 六章主干入口
| 想查什么 | 先翻哪里 | 关键 atom/专题 |

## 2. 高频考试入口
| 问题类型 | 第一跳 | 第二跳 |

## 3. 专题入口
| 专题 | 适用场景 | 索引文件 |

约束：极短，适合考试时第一眼定位。""",
    "01_中文关键词总索引.md": """生成“中文关键词总索引”。

结构：
# 中文关键词总索引

| 关键词 | 快速解释 | 第一位置 | 补充位置 |

约束：覆盖概念、主体、单证、流程、费用、责任、争议、事故、合同条款。""",
    "02_英文缩写总索引.md": """生成“英文缩写总索引”。

结构：
# 英文缩写总索引

| 缩写/英文 | 中文含义 | 所属类型 | 关键说明 | 位置 |

约束：优先提取 B/L、D/O、NOR、FOB、CIF、BAF 等英文缩写、英文术语、贸易术语、费用缩写、单证缩写。不要漏掉专题索引和补充卡片里的英文项。""",
    "03_问题总索引.md": """生成“问题总索引”。

结构：
# 问题总索引

| 考试问题 | 答题抓手 | 第一位置 | 补充位置 |

约束：问题应贴近开卷考试，包括定义题、比较题、流程题、案例处置题、责任判断题、计算/费用类题。""",
    "04_单证条款流程索引.md": """生成“单证条款流程索引”。

结构：
# 单证条款流程索引

## 1. 单证索引
| 单证 | 作用 | 风险/责任 | 位置 |

## 2. 条款索引
| 条款/规则 | 适用场景 | 关键点 | 位置 |

## 3. 流程索引
| 流程 | 核心步骤 | 位置 |

约束：突出提单、舱单、提货单、NOR、装卸时间、滞期速遣、索赔、班轮进出口、多式联运等。""",
    "05_打印顺序建议.md": """生成“打印顺序建议”。

结构：
# 打印顺序建议

## 1. 推荐打印顺序
| 顺序 | 文件/范围 | 用途 |

## 2. 考试时翻阅路径
| 场景 | 先翻 | 再翻 |

## 3. 可选压缩策略

约束：按总索引 -> 章节索引 -> 专题索引 -> atom 正文的层级给出实用顺序。""",
}


def create_client() -> OpenAI:
    load_dotenv()
    api_key = os.getenv("DASHSCOPE_API_KEY") or os.getenv("QWEN_API_KEY")
    if not api_key:
        raise RuntimeError("Set DASHSCOPE_API_KEY or QWEN_API_KEY before calling Qwen.")
    return OpenAI(api_key=api_key, base_url=os.getenv("DASHSCOPE_BASE_URL", DEFAULT_BASE_URL))


def response_text(response) -> str:
    text = getattr(response, "output_text", None)
    if text:
        return text
    parts: list[str] = []
    for item in getattr(response, "output", []) or []:
        if getattr(item, "type", None) != "message":
            continue
        for content in getattr(item, "content", []) or []:
            if getattr(content, "type", None) == "output_text":
                parts.append(content.text)
    return "\n".join(parts).strip()


def read_markdown_dir(path: Path, label: str) -> str:
    parts = []
    for md_file in sorted(path.glob("*.md")):
        parts.append(f"\n\n--- {label}: {md_file.name} ---\n\n{md_file.read_text(encoding='utf-8')}")
    return "".join(parts)


def read_cards(path: Path) -> str:
    if not path.exists():
        return "[]"
    cards = []
    with path.open("r", encoding="utf-8") as fh:
        for line in fh:
            if line.strip():
                cards.append(json.loads(line))
    compact = [
        {
            "atom_id": card.get("atom_id"),
            "title": card.get("title"),
            "source_type": card.get("source_type"),
            "topics": card.get("topics", []),
            "keywords_cn": card.get("keywords_cn", []),
            "keywords_en": card.get("keywords_en", []),
            "documents": card.get("documents", []),
            "processes": card.get("processes", []),
            "fees_or_trade_terms": card.get("fees_or_trade_terms", []),
            "questions": card.get("questions", []),
        }
        for card in cards
    ]
    return json.dumps(compact, ensure_ascii=False, indent=2)


def build_one(client: OpenAI, model: str, instruction: str, corpus: str) -> str:
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {
                "role": "user",
                "content": instruction
                + "\n\n输入材料如下：\n\n"
                + corpus
                + "\n\n约束：所有位置应指向章节索引、专题索引或 atom_id；不要引用 page_md、缓存、原始文件路径；不要复制大段正文。",
            },
        ],
        temperature=0.1,
        reasoning={"effort": "medium"},
    )
    return response_text(response)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--chapter-index-dir",
        default="国际航运代理理论与实务/整理与索引版本/01_章节索引",
        type=Path,
    )
    parser.add_argument(
        "--topic-index-dir",
        default="国际航运代理理论与实务/整理与索引版本/03_专题索引",
        type=Path,
    )
    parser.add_argument(
        "--cards",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/supplement_cards.jsonl",
        type=Path,
    )
    parser.add_argument(
        "--output-dir",
        default="国际航运代理理论与实务/整理与索引版本/00_总索引",
        type=Path,
    )
    parser.add_argument("--model", default=os.getenv("QWEN_INDEX_MODEL", DEFAULT_MODEL))
    parser.add_argument("--index-name", choices=list(MASTER_INDEXES))
    parser.add_argument("--print-only", action="store_true")
    args = parser.parse_args()

    targets = [args.index_name] if args.index_name else list(MASTER_INDEXES)
    for target in targets:
        print(f"Will build: {target}")
    if args.print_only:
        return

    corpus = (
        read_markdown_dir(args.chapter_index_dir, "章节索引")
        + read_markdown_dir(args.topic_index_dir, "专题索引")
        + "\n\n--- supplement_cards.jsonl ---\n\n"
        + read_cards(args.cards)
    )
    client = create_client()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for target in targets:
        print(f"Building master index: {target}", flush=True)
        markdown = build_one(client, args.model, MASTER_INDEXES[target], corpus)
        output_path = args.output_dir / target
        output_path.write_text(markdown.rstrip() + "\n", encoding="utf-8")
        print(f"Wrote {output_path}", flush=True)


if __name__ == "__main__":
    main()
