#!/usr/bin/env python3
"""Build a JSONL manifest of lecture atoms from course source files."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path


SOURCE_SUFFIXES = {
    "_原文.docx": "transcript",
    "_导读.docx": "guide",
    "_笔记.docx": "note",
    "_PPT.pdf": "slides",
}


def stable_id(text: str, length: int = 10) -> str:
    return hashlib.sha1(text.encode("utf-8")).hexdigest()[:length]


def normalize_stem(path: Path) -> tuple[str, str | None]:
    name = path.name
    for suffix, role in SOURCE_SUFFIXES.items():
        if name.endswith(suffix):
            return name[: -len(suffix)], role
    if path.suffix.lower() == ".pdf":
        return path.stem, "courseware"
    return path.stem, None


CHINESE_DIGITS = {
    "零": 0,
    "一": 1,
    "二": 2,
    "两": 2,
    "三": 3,
    "四": 4,
    "五": 5,
    "六": 6,
    "七": 7,
    "八": 8,
    "九": 9,
}


def chinese_number_to_int(text: str) -> int | None:
    if not text:
        return None
    if text == "十":
        return 10
    if "十" in text:
        left, _, right = text.partition("十")
        tens = CHINESE_DIGITS.get(left, 1) if left else 1
        ones = CHINESE_DIGITS.get(right, 0) if right else 0
        return tens * 10 + ones
    return CHINESE_DIGITS.get(text)


def order_number(text: str) -> int | None:
    digit_match = re.search(r"\d+", text)
    if digit_match:
        return int(digit_match.group(0))
    chinese_match = re.search(r"第([一二两三四五六七八九十]+)[章节讲节]", text)
    if chinese_match:
        return chinese_number_to_int(chinese_match.group(1))
    leading_chinese = re.search(r"^([一二两三四五六七八九十]+)[、.．_ -]", text)
    if leading_chinese:
        return chinese_number_to_int(leading_chinese.group(1))
    return None


def sort_key(path: Path) -> tuple[int, str]:
    numbered = order_number(path.name)
    if numbered is not None:
        return numbered, path.name
    match = re.search(r"[-_ ](\d+)", path.name)
    if match:
        return int(match.group(1)), path.name
    return 9999, path.name


def group_sort_key(chapter: str, base: str, files: list[tuple[str, Path]]) -> tuple[int, str, tuple[int, str]]:
    roles = {role for role, _ in files}
    chapter_order = order_number(chapter) if order_number(chapter) is not None else 9999
    kind_order = 0
    if roles == {"courseware"}:
        kind_order = 2 if "9页版本" in base else 1
    return chapter_order, chapter, (kind_order * 1_000_000 + sort_key(Path(base))[0], sort_key(Path(base))[1])


def lecture_title(base: str) -> str:
    title = re.sub(r"^王[学雪][锋峰]\s*\d+\s*", "", base)
    title = re.sub(r"（修改）|最新", "", title)
    title = re.sub(r"\s+", " ", title)
    return title.strip(" _-")


def build_manifest(course_dir: Path, output: Path) -> list[dict]:
    groups: dict[tuple[str, str], list[tuple[str, Path]]] = defaultdict(list)
    ignored_dirs = {
        (course_dir / "整理与索引版本").resolve(),
    }
    output = output.resolve()

    for path in sorted(course_dir.rglob("*")):
        resolved_path = path.resolve()
        if not path.is_file():
            continue
        if any(ignored_dir == resolved_path or ignored_dir in resolved_path.parents for ignored_dir in ignored_dirs):
            continue
        if resolved_path == output:
            continue
        if path.suffix.lower() not in {".docx", ".pdf"}:
            continue
        base, role = normalize_stem(path)
        if role is None:
            continue
        chapter = path.parent.relative_to(course_dir).as_posix()
        groups[(chapter, base)].append((role, path))

    now = datetime.now(timezone.utc).isoformat()
    records: list[dict] = []
    for index, ((chapter, base), files) in enumerate(
        sorted(
            groups.items(),
            key=lambda item: (
                group_sort_key(item[0][0], item[0][1], item[1]),
            ),
        ),
        start=1,
    ):
        rel_files = []
        roles_present = set()
        for role, path in sorted(files, key=lambda item: item[0]):
            roles_present.add(role)
            rel_files.append(
                {
                    "role": role,
                    "path": path.relative_to(course_dir.parent).as_posix(),
                    "bytes": path.stat().st_size,
                    "sha1": file_sha1(path),
                }
            )

        expected_roles = {"transcript", "guide", "slides"}
        if roles_present == {"courseware"}:
            expected_roles = {"courseware"}

        record = {
            "atom_id": f"L{index:03d}-{stable_id(chapter + '/' + base)}",
            "course": course_dir.name,
            "chapter": chapter,
            "atom_kind": "single_pdf_courseware" if roles_present == {"courseware"} else "lecture_bundle",
            "base_name": base,
            "title": lecture_title(base),
            "source_files": rel_files,
            "missing_roles": sorted(expected_roles - roles_present),
            "output_md": f"{course_dir.name}/整理与索引版本/02_讲义整理/L{index:03d}-{lecture_title(base)}.md",
            "created_at": now,
        }
        records.append(record)

    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("w", encoding="utf-8") as fh:
        for record in records:
            fh.write(json.dumps(record, ensure_ascii=False) + "\n")

    return records


def file_sha1(path: Path) -> str:
    digest = hashlib.sha1()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--course-dir",
        default="国际航运代理理论与实务",
        type=Path,
        help="Course directory containing chapter/source files.",
    )
    parser.add_argument(
        "--output",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
        help="Manifest JSONL path.",
    )
    args = parser.parse_args()

    records = build_manifest(args.course_dir, args.output)
    print(f"Wrote {len(records)} lecture atoms to {args.output}")


if __name__ == "__main__":
    main()
