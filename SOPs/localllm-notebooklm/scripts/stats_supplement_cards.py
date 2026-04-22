#!/usr/bin/env python3
"""Report topic and keyword coverage from supplement cards."""

from __future__ import annotations

import argparse
import csv
import json
from collections import Counter, defaultdict
from pathlib import Path


FIELDS = ["topics", "keywords_en", "keywords_cn"]


def read_cards(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def count_field(cards: list[dict], field: str) -> tuple[Counter, dict[str, list[str]]]:
    counter: Counter = Counter()
    refs: dict[str, list[str]] = defaultdict(list)
    for card in cards:
        for value in card.get(field, []) or []:
            key = str(value).strip()
            if not key:
                continue
            counter[key] += 1
            refs[key].append(f"{card['atom_id']} {card['title']}")
    return counter, refs


def write_csv(path: Path, field: str, counter: Counter, refs: dict[str, list[str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.writer(fh)
        writer.writerow([field, "count", "atoms"])
        for key, count in counter.most_common():
            writer.writerow([key, count, " | ".join(refs[key])])


def write_markdown(path: Path, cards: list[dict], stats: dict[str, tuple[Counter, dict[str, list[str]]]], limit: int) -> None:
    lines = [
        "# 补充资料卡片统计",
        "",
        f"- cards: {len(cards)}",
        "",
    ]
    for field, (counter, refs) in stats.items():
        lines.append(f"## {field}")
        lines.append("")
        lines.append("| 值 | 次数 | atom |")
        lines.append("|---|---:|---|")
        for key, count in counter.most_common(limit):
            lines.append(f"| {key} | {count} | {'<br>'.join(refs[key])} |")
        lines.append("")
    path.write_text("\n".join(lines).rstrip() + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--cards",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/supplement_cards.jsonl",
        type=Path,
    )
    parser.add_argument(
        "--output-dir",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据",
        type=Path,
    )
    parser.add_argument("--limit", type=int, default=50)
    args = parser.parse_args()

    cards = read_cards(args.cards)
    stats = {field: count_field(cards, field) for field in FIELDS}
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for field, (counter, refs) in stats.items():
        write_csv(args.output_dir / f"supplement_{field}_stats.csv", field, counter, refs)
        print(f"\n{field}")
        for key, count in counter.most_common(args.limit):
            print(f"{count:>3}  {key}")
    write_markdown(args.output_dir / "supplement_card_stats.md", cards, stats, args.limit)
    print(f"\nWrote stats to {args.output_dir}")


if __name__ == "__main__":
    main()
