#!/usr/bin/env python3
"""Summarize page_md sizes per lesson cache."""

from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path


def read_manifest(path: Path) -> dict[str, dict]:
    if not path.exists():
        return {}
    records: dict[str, dict] = {}
    with path.open("r", encoding="utf-8") as fh:
        for line in fh:
            if not line.strip():
                continue
            record = json.loads(line)
            records[record["atom_id"]] = record
    return records


def count_text(text: str) -> tuple[int, int, int]:
    chars = len(text)
    non_ws_chars = sum(1 for char in text if not char.isspace())
    lines = text.count("\n") + (1 if text else 0)
    return chars, non_ws_chars, lines


def estimate_tokens(chars: int, non_ws_chars: int, ratio: float) -> int:
    # Chinese-heavy Markdown is often near 1 token per 1-2 chars. This is a
    # conservative planning estimate, not an exact tokenizer count.
    base_chars = max(chars, non_ws_chars)
    return int(base_chars / ratio)


def risk_level(estimated_tokens: int, warn_tokens: int, danger_tokens: int) -> str:
    if estimated_tokens >= danger_tokens:
        return "danger"
    if estimated_tokens >= warn_tokens:
        return "warn"
    return "ok"


def collect_stats(
    cache_dir: Path,
    manifest_records: dict[str, dict],
    token_ratio: float,
    warn_tokens: int,
    danger_tokens: int,
) -> list[dict]:
    rows: list[dict] = []
    atom_ids = set(manifest_records)
    if cache_dir.exists():
        atom_ids.update(path.name for path in cache_dir.iterdir() if path.is_dir())

    for atom_id in sorted(atom_ids):
        lesson_dir = cache_dir / atom_id
        page_md_dir = lesson_dir / "page_md"
        page_files = sorted(page_md_dir.glob("*.md")) if page_md_dir.exists() else []
        chars = non_ws_chars = lines = 0
        largest_file = ""
        largest_chars = 0
        for page_file in page_files:
            text = page_file.read_text(encoding="utf-8", errors="replace")
            file_chars, file_non_ws_chars, file_lines = count_text(text)
            chars += file_chars
            non_ws_chars += file_non_ws_chars
            lines += file_lines
            if file_chars > largest_chars:
                largest_chars = file_chars
                largest_file = page_file.name

        estimated = estimate_tokens(chars, non_ws_chars, token_ratio)
        record = manifest_records.get(atom_id, {})
        rows.append(
            {
                "atom_id": atom_id,
                "title": record.get("title", ""),
                "chapter": record.get("chapter", ""),
                "page_md_files": len(page_files),
                "chars": chars,
                "non_ws_chars": non_ws_chars,
                "lines": lines,
                "estimated_tokens": estimated,
                "risk": risk_level(estimated, warn_tokens, danger_tokens),
                "largest_page_chars": largest_chars,
                "largest_page_file": largest_file,
                "has_page_md": bool(page_files),
            }
        )
    return rows


def print_table(rows: list[dict], limit: int | None) -> None:
    shown = rows[:limit] if limit else rows
    headers = [
        "risk",
        "atom_id",
        "page_md",
        "chars",
        "est_tokens",
        "title",
    ]
    print("\t".join(headers))
    for row in shown:
        print(
            "\t".join(
                [
                    row["risk"],
                    row["atom_id"],
                    str(row["page_md_files"]),
                    str(row["chars"]),
                    str(row["estimated_tokens"]),
                    row["title"],
                ]
            )
        )


def write_csv(path: Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(rows[0].keys()) if rows else [])
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--cache-dir",
        default="国际航运代理理论与实务/整理与索引版本/99_过程缓存",
        type=Path,
    )
    parser.add_argument(
        "--manifest",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument(
        "--output-csv",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/page_md_stats.csv",
        type=Path,
    )
    parser.add_argument("--token-ratio", type=float, default=1.5)
    parser.add_argument("--warn-tokens", type=int, default=80000)
    parser.add_argument("--danger-tokens", type=int, default=120000)
    parser.add_argument("--limit", type=int, default=20)
    args = parser.parse_args()

    records = read_manifest(args.manifest)
    rows = collect_stats(
        args.cache_dir,
        records,
        args.token_ratio,
        args.warn_tokens,
        args.danger_tokens,
    )
    rows.sort(key=lambda row: row["estimated_tokens"], reverse=True)
    write_csv(args.output_csv, rows)
    print_table(rows, args.limit)
    print(f"\nWrote CSV: {args.output_csv}")


if __name__ == "__main__":
    main()
