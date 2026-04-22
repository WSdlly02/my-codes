#!/usr/bin/env python3
"""Build topic indexes from chapter indexes and supplement cards."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path

from dotenv import load_dotenv
from openai import OpenAI


DEFAULT_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"
DEFAULT_MODEL = "qwen3.6-plus-2026-04-02"

TOPICS = [
    "航运基础",
    "船舶与港口",
    "货物与航线",
    "船舶代理",
    "货运代理",
    "班轮运输与提单",
    "航次租船",
    "定期租船与光船租船",
    "多式联运与集装箱",
    "货运事故与索赔",
    "海上货运公约",
    "争议解决",
    "费用运价与附加费",
    "英文缩写与术语",
]

SYSTEM_PROMPT = """你是国际航运代理课程专题索引编制助手。
目标是把章节索引和补充资料卡片整合为横向专题索引，用于开卷考试快速定位。
专题索引不替代 atom 正文，只提供跨章节、跨资料导航。只依据输入材料，不编造。输出中文 Markdown。"""

TOPIC_PROMPT = """请为专题“{topic}”生成可打印专题索引。

必须输出以下结构：
# 专题：{topic}

## 1. 专题速查
| 查什么 | 直接答案/定位提示 | 主位置 | 补充位置 |

## 2. 主干章节入口
| 内容 | 所在章节 | atom/小节 |

## 3. 推荐视频补充
| 补充点 | atom | 为什么有用 |

## 4. 课件压缩概念
| 概念/框架 | atom | 可用于什么题 |

## 5. 英文缩写与术语
| 缩写/英文 | 中文 | 说明 | 位置 |

## 6. 易混淆点
| 易混点 | 辨析 | 位置 |

## 7. 高频问题
| 问题 | 答题要点 | 位置 |

约束：
- 主干章节入口来自章节索引；补充材料来自 supplement cards。
- 所有位置必须写章节名、atom_id 或补充 atom_id。
- 如果某部分材料不足，可以写“本专题未明显出现”，不要编造。
- 英文缩写与术语要尽量提取，尤其费用、贸易术语、单证缩写。
- 不要复制大段正文。

章节索引：
{chapter_indexes}

补充资料卡片：
{cards}
"""


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


def read_chapter_indexes(path: Path) -> str:
    parts = []
    for chapter_file in sorted(path.glob("*.md")):
        parts.append(f"\n\n--- {chapter_file.name} ---\n\n{chapter_file.read_text(encoding='utf-8')}")
    return "".join(parts)


def read_cards(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def cards_for_topic(cards: list[dict], topic: str) -> list[dict]:
    if topic == "英文缩写与术语":
        return [card for card in cards if card.get("keywords_en")]
    return [card for card in cards if topic in card.get("topics", [])]


def unknown_topics(cards: list[dict]) -> list[str]:
    known = set(TOPICS)
    found = {topic for card in cards for topic in card.get("topics", [])}
    return sorted(found - known)


def build_topic(client: OpenAI, model: str, topic: str, chapter_indexes: str, cards: list[dict]) -> str:
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {
                "role": "user",
                "content": TOPIC_PROMPT.format(
                    topic=topic,
                    chapter_indexes=chapter_indexes,
                    cards=json.dumps(cards, ensure_ascii=False, indent=2),
                ),
            },
        ],
        temperature=0.1,
        reasoning={"effort": "medium"},
    )
    return response_text(response)


def safe_filename(topic: str) -> str:
    return topic.replace("/", "_") + ".md"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--chapter-index-dir",
        default="国际航运代理理论与实务/整理与索引版本/01_章节索引",
        type=Path,
    )
    parser.add_argument(
        "--cards",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/supplement_cards.jsonl",
        type=Path,
    )
    parser.add_argument(
        "--output-dir",
        default="国际航运代理理论与实务/整理与索引版本/03_专题索引",
        type=Path,
    )
    parser.add_argument("--model", default=os.getenv("QWEN_INDEX_MODEL", DEFAULT_MODEL))
    parser.add_argument("--topic", choices=TOPICS)
    parser.add_argument("--print-only", action="store_true")
    args = parser.parse_args()

    topics = [args.topic] if args.topic else TOPICS
    cards = read_cards(args.cards)
    missing_topics = unknown_topics(cards)
    if missing_topics:
        print("Warning: supplement cards contain topics not listed in TOPICS:")
        for topic in missing_topics:
            print(f"  - {topic}")
    for topic in topics:
        selected = cards_for_topic(cards, topic)
        print(f"{topic}: {len(selected)} supplement card(s)")
        for card in selected:
            print(f"  - {card['atom_id']} {card['title']}")
    if args.print_only:
        return

    chapter_indexes = read_chapter_indexes(args.chapter_index_dir)
    client = create_client()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for topic in topics:
        selected = cards_for_topic(cards, topic)
        print(f"Building topic index: {topic}", flush=True)
        markdown = build_topic(client, args.model, topic, chapter_indexes, selected)
        output_path = args.output_dir / safe_filename(topic)
        output_path.write_text(markdown.rstrip() + "\n", encoding="utf-8")
        print(f"Wrote {output_path}", flush=True)


if __name__ == "__main__":
    main()
