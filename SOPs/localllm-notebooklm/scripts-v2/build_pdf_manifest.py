#!/usr/bin/env python3
"""Build the PaddleOCR manifest from normalized PDF records."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build manifest.jsonl for batch-paddle-ocr from normalized_files.jsonl."
    )
    parser.add_argument("course_dir", type=Path, help="Course root directory.")
    parser.add_argument(
        "--normalized",
        type=Path,
        help=(
            "Path to normalized_files.jsonl. Defaults to "
            "<course_dir>/整理与索引版本/99_过程缓存/normalized_files.jsonl."
        ),
    )
    parser.add_argument(
        "--output",
        type=Path,
        help=(
            "Output manifest path. Defaults to "
            "<course_dir>/整理与索引版本/99_过程缓存/manifest.jsonl."
        ),
    )
    parser.add_argument(
        "--include-failed",
        action="store_true",
        help="Include failed normalization records for inspection. Not for OCR runs.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    course_dir = args.course_dir.resolve()
    if not course_dir.is_dir():
        print(f"ERROR: course_dir is not a directory: {course_dir}", file=sys.stderr)
        return 2

    cache_dir = course_dir / "整理与索引版本" / "99_过程缓存"
    normalized_path = args.normalized.resolve() if args.normalized else cache_dir / "normalized_files.jsonl"
    output_path = args.output.resolve() if args.output else cache_dir / "manifest.jsonl"

    if not normalized_path.is_file():
        print(f"ERROR: normalized file not found: {normalized_path}", file=sys.stderr)
        return 2

    rows = read_jsonl(normalized_path)
    manifest_rows = []
    skipped_failed = 0
    skipped_missing_pdf = 0

    for row in rows:
        status = row.get("status")
        pdf_path = Path(row.get("pdf_path", ""))
        if status != "ok" and not args.include_failed:
            skipped_failed += 1
            continue
        if status == "ok" and not pdf_path.is_file():
            skipped_missing_pdf += 1
            print(f"WARNING: normalized PDF missing: {pdf_path}", file=sys.stderr)
            continue

        manifest_rows.append(
            {
                "id": require_field(row, "pdf_id"),
                "pdf_name": require_field(row, "pdf_name"),
                "pdf_path": str(pdf_path),
            }
        )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8") as fh:
        for row in manifest_rows:
            fh.write(json.dumps(row, ensure_ascii=False) + "\n")

    print(
        "Done. "
        f"input={len(rows)}, output={len(manifest_rows)}, "
        f"skipped_failed={skipped_failed}, skipped_missing_pdf={skipped_missing_pdf}, "
        f"manifest={output_path}"
    )
    return 0


def read_jsonl(path: Path) -> list[dict]:
    rows: list[dict] = []
    with path.open("r", encoding="utf-8") as fh:
        for line_no, line in enumerate(fh, start=1):
            line = line.strip()
            if not line:
                continue
            try:
                value = json.loads(line)
            except json.JSONDecodeError as exc:
                raise SystemExit(f"ERROR: invalid JSON at {path}:{line_no}: {exc}") from exc
            if not isinstance(value, dict):
                raise SystemExit(f"ERROR: JSONL row is not an object at {path}:{line_no}")
            rows.append(value)
    return rows


def require_field(row: dict, field: str) -> str:
    value = row.get(field)
    if not isinstance(value, str) or not value:
        raise SystemExit(f"ERROR: normalized record missing string field: {field}")
    return value


if __name__ == "__main__":
    raise SystemExit(main())
