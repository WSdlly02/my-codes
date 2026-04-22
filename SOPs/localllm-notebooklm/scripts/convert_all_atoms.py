#!/usr/bin/env python3
"""Run convert_atom.py for atoms listed in the manifest."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path


def read_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def select_atoms(records: list[dict], only_missing: bool, course_root: Path) -> list[dict]:
    if not only_missing:
        return records
    return [record for record in records if not (course_root / record["output_md"]).exists()]


def chunks(items: list[dict], size: int):
    for offset in range(0, len(items), size):
        yield items[offset : offset + size]


def command_for_record(record: dict, args, passthrough: list[str]) -> list[str]:
    command = [
        sys.executable,
        "scripts/convert_atom.py",
        record["atom_id"],
        "--manifest",
        str(args.manifest),
        "--course-root",
        str(args.course_root),
    ]
    if args.reuse_page_md:
        command.append("--reuse-page-md")
    if args.ocr_only:
        command.append("--ocr-only")
    command.extend(passthrough)
    return command


def run_one(record: dict, args, passthrough: list[str], batch_dir: Path) -> tuple[dict, int, Path]:
    command = command_for_record(record, args, passthrough)
    batch_dir.mkdir(parents=True, exist_ok=True)
    log_path = batch_dir / f"{record['atom_id']}.log"
    with log_path.open("w", encoding="utf-8") as log:
        log.write("$ " + " ".join(command) + "\n\n")
        log.flush()
        process = subprocess.Popen(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
            env={**os.environ, "PYTHONUNBUFFERED": "1"},
        )
        assert process.stdout is not None
        for line in process.stdout:
            log.write(line)
            log.flush()
            print(f"[{record['atom_id']}] {line}", end="", flush=True)
        returncode = process.wait()
    return record, returncode, log_path


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument("--course-root", default=".", type=Path)
    parser.add_argument("--start-at", help="Start from this atom_id.")
    parser.add_argument("--limit", type=int, help="Maximum number of atoms to process.")
    parser.add_argument(
        "--only-missing",
        action="store_true",
        help="Skip atoms whose final Markdown already exists.",
    )
    parser.add_argument(
        "--reuse-page-md",
        action="store_true",
        help="Pass --reuse-page-md to convert_atom.py.",
    )
    parser.add_argument(
        "--ocr-only",
        action="store_true",
        help="Pass --ocr-only to convert_atom.py.",
    )
    parser.add_argument(
        "--print-only",
        action="store_true",
        help="Print commands without running them.",
    )
    parser.add_argument(
        "--parallel",
        type=int,
        default=1,
        help="Number of convert_atom.py processes to run per batch. Default: 1.",
    )
    parser.add_argument(
        "--log-dir",
        default="国际航运代理理论与实务/整理与索引版本/99_过程缓存/convert_all_logs",
        type=Path,
        help="Directory for per-atom conversion logs.",
    )
    args, passthrough = parser.parse_known_args()
    if args.parallel < 1:
        raise SystemExit("--parallel must be >= 1")

    records = select_atoms(read_manifest(args.manifest), args.only_missing, args.course_root)
    if args.start_at:
        start_indexes = [index for index, record in enumerate(records) if record["atom_id"] == args.start_at]
        if not start_indexes:
            raise SystemExit(f"No atom_id matched --start-at: {args.start_at}")
        records = records[start_indexes[0] :]
    if args.limit is not None:
        records = records[: args.limit]

    if args.print_only:
        for index, record in enumerate(records, start=1):
            command = command_for_record(record, args, passthrough)
            print(f"[{index}/{len(records)}] {record['atom_id']} {record['title']}", flush=True)
            print(" ".join(command), flush=True)
        return

    completed = 0
    for batch_index, batch in enumerate(chunks(records, args.parallel), start=1):
        print(
            f"Batch {batch_index}: running {len(batch)} atom(s), "
            f"{completed}/{len(records)} completed",
            flush=True,
        )
        batch_dir = args.log_dir / f"batch_{batch_index:03d}"
        failures: list[tuple[dict, int, Path]] = []
        with ThreadPoolExecutor(max_workers=len(batch)) as executor:
            future_map = {
                executor.submit(run_one, record, args, passthrough, batch_dir): record
                for record in batch
            }
            for future in as_completed(future_map):
                record, returncode, log_path = future.result()
                if returncode == 0:
                    completed += 1
                    print(f"OK   {record['atom_id']} {record['title']} -> {log_path}", flush=True)
                else:
                    failures.append((record, returncode, log_path))
                    print(
                        f"FAIL {record['atom_id']} {record['title']} "
                        f"(exit {returncode}) -> {log_path}",
                        flush=True,
                    )
        if failures:
            raise SystemExit(f"Stopping after batch {batch_index}: {len(failures)} failure(s).")

    print(f"Completed {completed} atom(s).", flush=True)


if __name__ == "__main__":
    main()
