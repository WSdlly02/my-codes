#!/usr/bin/env python3
"""Build JSONL cards for recommended-video and courseware atoms."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path

from dotenv import load_dotenv
from openai import OpenAI


DEFAULT_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"
DEFAULT_MODEL = "qwen3.6-plus-2026-04-02"

SYSTEM_PROMPT = """你是国际航运代理课程资料索引编制助手。
目标是把非主干 atom 提炼为机器可读的补充资料卡片，用于后续专题索引。
只依据输入 atom 内容，不编造。输出严格 JSON。"""

CARD_PROMPT = """请把下面 atom Markdown 提炼为 JSON 卡片。

atom_id: {atom_id}
title: {title}
chapter: {chapter}
atom_kind: {atom_kind}
source_type: {source_type}

JSON 字段必须为：
{{
  "atom_id": "{atom_id}",
  "title": "{title}",
  "source_type": "{source_type}",
  "priority": "core|supplement|overview",
  "one_line": "",
  "topics": [],
  "keywords_cn": [],
  "keywords_en": [],
  "concepts": [],
  "documents": [],
  "processes": [],
  "legal_rules": [],
  "fees_or_trade_terms": [],
  "case_points": [],
  "questions": [],
  "best_used_for": []
}}

要求：
- 只输出 JSON，不要 Markdown，不要解释。
- keywords_en 专门放英文术语、英文缩写、贸易术语和费用缩写，如 B/L、D/O、NOR、FOB、CIF、BAF 等。
- topics 使用简短中文专题名，例如：航运基础、船舶与港口、货物与航线、船舶代理、货运代理、班轮运输与提单、航次租船、定期租船与光船租船、多式联运与集装箱、货运事故与索赔、海上货运公约、争议解决、费用运价与附加费、英文缩写与术语。
- recommended video 用 priority=supplement；单 PDF 课件/9页版本多用于 overview。
- 数组元素要短，便于后续索引聚合。

atom Markdown：

{atom_text}
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


def read_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def source_type(record: dict) -> str:
    if record["atom_kind"] == "single_pdf_courseware":
        return "courseware"
    return "recommended_video"


def supplement_records(records: list[dict]) -> list[dict]:
    return [record for record in records if record["chapter"] == "资料"]


def load_existing_cards(path: Path) -> dict[str, dict]:
    if not path.exists():
        return {}
    cards: dict[str, dict] = {}
    with path.open("r", encoding="utf-8") as fh:
        for line in fh:
            if not line.strip():
                continue
            card = json.loads(line)
            cards[card["atom_id"]] = card
    return cards


def parse_json_object(text: str) -> dict:
    cleaned = text.strip()
    if cleaned.startswith("```"):
        cleaned = cleaned.strip("`")
        if cleaned.startswith("json"):
            cleaned = cleaned[4:].strip()
    start = cleaned.find("{")
    end = cleaned.rfind("}")
    if start == -1 or end == -1:
        raise ValueError(f"No JSON object found in response: {text[:200]}")
    return json.loads(cleaned[start : end + 1])


def build_card(client: OpenAI, model: str, record: dict, course_root: Path) -> dict:
    atom_path = course_root / record["output_md"]
    atom_text = atom_path.read_text(encoding="utf-8")
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {
                "role": "user",
                "content": CARD_PROMPT.format(
                    atom_id=record["atom_id"],
                    title=record["title"],
                    chapter=record["chapter"],
                    atom_kind=record["atom_kind"],
                    source_type=source_type(record),
                    atom_text=atom_text,
                ),
            },
        ],
        temperature=0,
        reasoning={"effort": "medium"},
    )
    card = parse_json_object(response_text(response))
    card["atom_id"] = record["atom_id"]
    card["title"] = record["title"]
    card["source_type"] = source_type(record)
    return card


def write_cards(path: Path, cards: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as fh:
        for card in cards:
            fh.write(json.dumps(card, ensure_ascii=False) + "\n")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument("--course-root", default=".", type=Path)
    parser.add_argument(
        "--output",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/supplement_cards.jsonl",
        type=Path,
    )
    parser.add_argument("--model", default=os.getenv("QWEN_INDEX_MODEL", DEFAULT_MODEL))
    parser.add_argument("--only-missing", action="store_true")
    parser.add_argument("--limit", type=int)
    parser.add_argument("--print-only", action="store_true")
    args = parser.parse_args()

    records = supplement_records(read_manifest(args.manifest))
    if args.limit is not None:
        records = records[: args.limit]
    existing = load_existing_cards(args.output)
    if args.only_missing:
        records = [record for record in records if record["atom_id"] not in existing]

    print(f"Supplement atoms to card: {len(records)}")
    for record in records:
        print(f"- {record['atom_id']} {source_type(record)} {record['title']}")
    if args.print_only:
        return

    client = create_client()
    cards = existing.copy()
    for record in records:
        print(f"Building card: {record['atom_id']} {record['title']}", flush=True)
        cards[record["atom_id"]] = build_card(client, args.model, record, args.course_root)
    ordered_cards = [cards[record["atom_id"]] for record in supplement_records(read_manifest(args.manifest)) if record["atom_id"] in cards]
    write_cards(args.output, ordered_cards)
    print(f"Wrote {len(ordered_cards)} cards to {args.output}")


if __name__ == "__main__":
    main()
