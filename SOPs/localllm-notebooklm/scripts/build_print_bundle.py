#!/usr/bin/env python3
"""Assemble print-ready Markdown bundles in the desired order."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


COURSE_DIR = Path("国际航运代理理论与实务")
BASE_DIR = COURSE_DIR / "整理与索引版本"


def read_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def page_break() -> str:
    return "\n\n\\newpage\n\n"


def read_md(path: Path) -> str:
    text = path.read_text(encoding="utf-8").strip()
    return text + "\n"


def read_md_with_title_prefix(path: Path, title_prefix: str | None = None) -> str:
    text = path.read_text(encoding="utf-8").strip()
    if not title_prefix:
        return text + "\n"
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if line.startswith("# "):
            title = line[2:].strip()
            if not title.startswith(title_prefix):
                lines[index] = f"# {title_prefix} {title}"
            break
    return "\n".join(lines).strip() + "\n"


def section(title: str) -> str:
    return f"# {title}\n"


def atom_sort_key(path: Path) -> int:
    match = re.search(r"L(\d{3})", path.name)
    return int(match.group(1)) if match else 9999


def topic_order(path: Path) -> tuple[int, str]:
    order = {
        "航运基础": 1,
        "船舶与港口": 2,
        "货物与航线": 3,
        "船舶代理": 4,
        "货运代理": 5,
        "班轮运输与提单": 6,
        "航次租船": 7,
        "定期租船与光船租船": 8,
        "多式联运与集装箱": 9,
        "货运事故与索赔": 10,
        "海上货运公约": 11,
        "争议解决": 12,
        "费用运价与附加费": 13,
        "英文缩写与术语": 14,
    }
    return order.get(path.stem, 999), path.name


def topic_number(path: Path) -> int:
    return topic_order(path)[0]


def topic_catalog(paths: list[Path]) -> str:
    lines = ["# 专题索引目录", "", "| 编号 | 专题 | 文件 |", "|---:|---|---|"]
    for path in paths:
        number = topic_number(path)
        label = f"T{number:02d}" if number < 999 else "T??"
        lines.append(f"| {label} | {path.stem} | {path.name} |")
    return "\n".join(lines).rstrip() + "\n"


def build_bundle(base_dir: Path, manifest_path: Path) -> str:
    manifest = read_manifest(manifest_path)
    atom_paths = sorted((base_dir / "02_讲义整理").glob("L*.md"), key=atom_sort_key)
    tutorial_ids = {record["atom_id"] for record in manifest if record["chapter"].startswith("第")}
    tutorial_prefixes = {atom_id.split("-", 1)[0] for atom_id in tutorial_ids}

    tutorial_atoms = [path for path in atom_paths if path.stem.split("-", 1)[0] in tutorial_prefixes]
    supplement_atoms = [path for path in atom_paths if path not in tutorial_atoms]

    parts: list[str] = [
        "---\n",
        "title: 国际航运代理理论与实务 开卷考试索引资料\n",
        "lang: zh-CN\n",
        "documentclass: ctexart\n",
        "geometry: margin=1.6cm\n",
        "---\n\n",
        "# 国际航运代理理论与实务 开卷考试索引资料\n",
        "\n\\tableofcontents\n",
    ]

    topic_paths = sorted((base_dir / "03_专题索引").glob("*.md"), key=topic_order)

    groups = [
        ("总索引", sorted((base_dir / "00_总索引").glob("*.md"))),
        ("章节索引", sorted((base_dir / "01_章节索引").glob("*.md"))),
        ("教程正文", tutorial_atoms),
        ("专题索引", topic_paths),
        ("补充正文", supplement_atoms),
    ]

    for group_title, paths in groups:
        parts.append(page_break())
        parts.append(section(group_title))
        if group_title == "专题索引":
            parts.append("\n")
            parts.append(topic_catalog(paths))
        for path in paths:
            parts.append(page_break())
            if group_title == "专题索引":
                number = topic_number(path)
                prefix = f"T{number:02d}" if number < 999 else None
                parts.append(read_md_with_title_prefix(path, prefix))
            else:
                parts.append(read_md(path))

    return "".join(parts).rstrip() + "\n"


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base-dir", default=BASE_DIR, type=Path)
    parser.add_argument(
        "--manifest",
        default=BASE_DIR / "04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument(
        "--output",
        default=BASE_DIR / "99_打印版/final_print.md",
        type=Path,
    )
    args = parser.parse_args()

    markdown = build_bundle(args.base_dir, args.manifest)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(markdown, encoding="utf-8")
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
