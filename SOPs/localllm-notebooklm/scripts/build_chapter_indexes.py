#!/usr/bin/env python3
"""Build chapter indexes from tutorial atom Markdown files."""

from __future__ import annotations

import argparse
import json
import os
from collections import defaultdict
from pathlib import Path

from dotenv import load_dotenv
from openai import OpenAI


DEFAULT_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"
DEFAULT_MODEL = "qwen3.6-plus-2026-04-02"
CHAPTER_ORDER = {
    "第一章": 1,
    "第二章": 2,
    "第三章": 3,
    "第四章": 4,
    "第五章": 5,
    "第六章": 6,
}

SYSTEM_PROMPT = """你是国际航运代理课程资料索引编制助手。
目标是把若干 atom 讲义合成为章节索引。章节索引用于开卷考试时快速定位，不替代 atom 正文。
只依据输入 atom 内容，不编造。输出中文 Markdown。"""

CHAPTER_PROMPT = """请根据下面同一章节的 atom Markdown，生成一个可打印的章节索引。

章节：{chapter}

必须输出以下结构：
# {chapter}

## 1. 本章一句话

## 2. 本章知识地图

## 3. Atom 路由表
| 查什么 | 先看哪个 atom | 为什么 |

## 4. 核心概念索引
| 概念 | 快速解释 | atom |

## 5. 单证、条款、主体与责任索引
| 对象 | 类型 | 关键规则/责任 | atom |

## 6. 流程与操作索引
| 流程/操作 | 步骤要点 | atom |

## 7. 英文缩写与术语
| 缩写/英文 | 中文含义 | 相关规则/作用 | atom |

## 8. 高频问题索引
| 问题 | 答题要点 | atom |

## 9. 易混淆点
| 易混点 | 辨析 | atom |

## 10. 本章打印与查阅建议

约束：
- 章节索引是“第二层索引”，不要复制 atom 大段正文。
- 所有定位必须指向具体 atom，如 L001、L002。
- 英文缩写、英文术语、贸易术语、费用缩写必须尽量提取，例如 BAF、FOB、CIF、B/L、D/O、NOR 等；没有则写“本章未明显出现”。
- 优先组织成表格，便于打印检索。
- 不要引用 page_md、缓存、原始文件路径。
- 不要写泛泛学习建议。

输入 atom：

{atom_text}
"""


def create_client() -> OpenAI:
    load_dotenv()
    api_key = os.getenv("DASHSCOPE_API_KEY") or os.getenv("QWEN_API_KEY")
    if not api_key:
        raise RuntimeError("Set DASHSCOPE_API_KEY or QWEN_API_KEY before calling Qwen.")
    return OpenAI(api_key=api_key, base_url=os.getenv("DASHSCOPE_BASE_URL", DEFAULT_BASE_URL))


def read_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def tutorial_records(records: list[dict]) -> list[dict]:
    return [record for record in records if record["chapter"].startswith("第")]


def atom_id_prefix(atom_id: str) -> str:
    return atom_id.split("-", 1)[0]


def read_atom_text(course_root: Path, record: dict) -> str:
    output_path = course_root / record["output_md"]
    text = output_path.read_text(encoding="utf-8")
    return f"\n\n--- ATOM {record['atom_id']} {record['title']} ---\n\n{text}"


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


def build_one_chapter(client: OpenAI, model: str, chapter: str, records: list[dict], course_root: Path) -> str:
    atom_text = "".join(read_atom_text(course_root, record) for record in records)
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": CHAPTER_PROMPT.format(chapter=chapter, atom_text=atom_text)},
        ],
        temperature=0.1,
        reasoning={"effort": "medium"},
    )
    return response_text(response)


def chapter_filename(chapter: str) -> str:
    order = chapter_order(chapter)
    safe = chapter.replace("/", "_")
    return f"{order:02d}_{safe}.md"


def chapter_order(chapter: str) -> int:
    for prefix, order in CHAPTER_ORDER.items():
        if chapter.startswith(prefix):
            return order
    return 999


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument("--course-root", default=".", type=Path)
    parser.add_argument(
        "--output-dir",
        default="国际航运代理理论与实务/整理与索引版本/01_章节索引",
        type=Path,
    )
    parser.add_argument("--model", default=os.getenv("QWEN_INDEX_MODEL", DEFAULT_MODEL))
    parser.add_argument("--chapter", help="Only build one chapter by exact chapter name.")
    parser.add_argument("--print-only", action="store_true")
    args = parser.parse_args()

    records_by_chapter: dict[str, list[dict]] = defaultdict(list)
    for record in tutorial_records(read_manifest(args.manifest)):
        if args.chapter and record["chapter"] != args.chapter:
            continue
        records_by_chapter[record["chapter"]].append(record)

    for chapter_records in records_by_chapter.values():
        chapter_records.sort(key=lambda record: atom_id_prefix(record["atom_id"]))

    for chapter, records in sorted(records_by_chapter.items(), key=lambda item: chapter_order(item[0])):
        print(f"{chapter}: {len(records)} atom(s)")
        for record in records:
            print(f"  - {record['atom_id']} {record['title']}")
    if args.print_only:
        return

    client = create_client()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for chapter, records in sorted(records_by_chapter.items(), key=lambda item: chapter_order(item[0])):
        print(f"Building chapter index: {chapter}", flush=True)
        markdown = build_one_chapter(client, args.model, chapter, records, args.course_root)
        output_path = args.output_dir / chapter_filename(chapter)
        output_path.write_text(markdown.rstrip() + "\n", encoding="utf-8")
        print(f"Wrote {output_path}", flush=True)


if __name__ == "__main__":
    main()
