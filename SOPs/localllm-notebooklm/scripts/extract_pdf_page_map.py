#!/usr/bin/env python3
"""Extract Lxxx and major section starting pages from a rendered PDF."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import tempfile
from pathlib import Path


L_RE = re.compile(r"\b(L\d{3})(?:-[0-9a-f]{10})?\b")
SECTION_RE = re.compile(r"^(总索引|章节索引|教程正文|专题索引|补充正文)$", re.MULTILINE)
L_TOC_RE = re.compile(r"\b(L\d{3})(?:-[0-9A-Fa-f]{10})?\b.*?(\d{1,4})\s*$")
SECTION_TOC_RE = re.compile(r"\b(总索引|章节索引|教程正文|专题索引|补充正文)\b.*?(\d{1,4})\s*$")


def run_pdftotext(pdf: Path, output_txt: Path) -> None:
    subprocess.run(["pdftotext", "-layout", str(pdf), str(output_txt)], check=True)


def split_pages(text: str) -> list[str]:
    return text.split("\f")


def parse_trailing_page_maps(text: str) -> tuple[dict[str, int], dict[str, int]]:
    l_pages: dict[str, int] = {}
    section_pages: dict[str, int] = {}
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line or line.startswith("|"):
            continue
        l_match = L_TOC_RE.search(line)
        if l_match:
            l_code = l_match.group(1).upper()
            page = int(l_match.group(2))
            # Ignore accidental tiny numbers from non-TOC contexts. Real atom
            # starts are never before the first index pages in this bundle.
            if page >= 10:
                l_pages.setdefault(l_code, page)
        section_match = SECTION_TOC_RE.search(line)
        if section_match:
            page = int(section_match.group(2))
            if page >= 1:
                section_pages.setdefault(section_match.group(1), page)
    return l_pages, section_pages


def extract_map(pdf: Path) -> tuple[dict[str, int], dict[str, int]]:
    with tempfile.TemporaryDirectory() as tmp:
        txt_path = Path(tmp) / "pdf.txt"
        run_pdftotext(pdf, txt_path)
        text = txt_path.read_text(encoding="utf-8", errors="replace")

    l_pages, section_pages = parse_trailing_page_maps(text)
    if l_pages:
        return l_pages, section_pages

    pages = split_pages(text)
    l_pages: dict[str, int] = {}
    section_pages: dict[str, int] = {}
    for index, page in enumerate(pages, start=1):
        for match in L_RE.finditer(page):
            l_pages.setdefault(match.group(1), index)
        for match in SECTION_RE.finditer(page):
            section_pages.setdefault(match.group(1), index)
    return l_pages, section_pages


def write_markdown(path: Path, l_pages: dict[str, int], section_pages: dict[str, int]) -> None:
    lines = ["# L编号页码表", ""]
    if section_pages:
        lines.extend(["## 分区页码", "", "| 分区 | 页码 |", "|---|---:|"])
        for section, page in section_pages.items():
            lines.append(f"| {section} | {page} |")
        lines.append("")
    lines.extend(["## Atom 起始页", "", "| L编号 | 页码 |", "|---|---:|"])
    for l_code, page in sorted(l_pages.items()):
        lines.append(f"| {l_code} | {page} |")
    path.write_text("\n".join(lines).rstrip() + "\n", encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("pdf", type=Path)
    parser.add_argument(
        "--output-md",
        default="国际航运代理理论与实务/整理与索引版本/00_总索引/06_L编号页码表.md",
        type=Path,
    )
    parser.add_argument(
        "--output-json",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/page_map.json",
        type=Path,
    )
    args = parser.parse_args()

    l_pages, section_pages = extract_map(args.pdf)
    args.output_md.parent.mkdir(parents=True, exist_ok=True)
    args.output_json.parent.mkdir(parents=True, exist_ok=True)
    write_markdown(args.output_md, l_pages, section_pages)
    args.output_json.write_text(
        json.dumps({"sections": section_pages, "atoms": l_pages}, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )
    print(f"Found {len(l_pages)} L-code page entries")
    print(f"Wrote {args.output_md}")
    print(f"Wrote {args.output_json}")


if __name__ == "__main__":
    main()
