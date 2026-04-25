#!/usr/bin/env python3
"""Convert one lecture atom from source files to an indexed Markdown note."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

from llmcall import (
    call_qwen_page_ocr,
    call_qwen_synthesis_from_page_md,
    create_qwen_client,
)


@dataclass
class RenderedPage:
    source_role: str
    source_path: Path
    page_no: int
    image_path: Path


def run(command: list[str]) -> None:
    subprocess.run(command, check=True)


def require_tool(name: str) -> str:
    found = shutil.which(name)
    if not found:
        raise RuntimeError(f"Missing required tool: {name}")
    return found


def read_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def find_atom(records: list[dict], selector: str) -> dict:
    matches = [
        record
        for record in records
        if selector == record["atom_id"]
        or selector in record["title"]
        or selector in record["base_name"]
    ]
    if not matches:
        raise SystemExit(f"No atom matched selector: {selector}")
    if len(matches) > 1:
        print("Multiple atoms matched; please use atom_id:", file=sys.stderr)
        for record in matches:
            print(
                f"- {record['atom_id']} {record['chapter']} {record['title']}",
                file=sys.stderr,
            )
        raise SystemExit(2)
    return matches[0]


def convert_docx_to_pdf(source: Path, work_dir: Path) -> Path:
    require_tool("libreoffice")
    out_dir = work_dir / "pdf"
    profile_dir = work_dir / "libreoffice_profile" / source.stem
    out_dir.mkdir(parents=True, exist_ok=True)
    profile_dir.mkdir(parents=True, exist_ok=True)
    expected = out_dir / f"{source.stem}.pdf"
    if not expected.exists():
        run(
            [
                "libreoffice",
                "--headless",
                "--nologo",
                "--nofirststartwizard",
                f"-env:UserInstallation={profile_dir.resolve().as_uri()}",
                "--convert-to",
                "pdf",
                "--outdir",
                str(out_dir),
                str(source),
            ]
        )
    if not expected.exists():
        raise RuntimeError(f"LibreOffice did not create expected PDF: {expected}")
    return expected


def render_pdf_to_images(
    source_pdf: Path,
    work_dir: Path,
    role: str,
    original_source: Path,
    dpi: int,
    max_pages: int | None,
) -> list[RenderedPage]:
    require_tool("pdftoppm")
    image_dir = work_dir / "pages" / source_pdf.stem
    image_dir.mkdir(parents=True, exist_ok=True)
    prefix = image_dir / "page"

    command = ["pdftoppm", "-png", "-r", str(dpi)]
    if max_pages:
        command.extend(["-f", "1", "-l", str(max_pages)])
    command.extend([str(source_pdf), str(prefix)])
    if not list(image_dir.glob("page-*.png")):
        run(command)

    pages: list[RenderedPage] = []
    for index, image in enumerate(sorted(image_dir.glob("page-*.png")), start=1):
        pages.append(RenderedPage(role, original_source, index, image))
    return pages


def render_source_file(
    source: Path,
    role: str,
    work_dir: Path,
    dpi: int,
    max_pages_per_file: int | None,
) -> list[RenderedPage]:
    suffix = source.suffix.lower()
    if suffix == ".pdf":
        pdf = source
    elif suffix == ".docx":
        pdf = convert_docx_to_pdf(source, work_dir)
    else:
        raise ValueError(f"Unsupported source file: {source}")
    return render_pdf_to_images(pdf, work_dir, role, source, dpi, max_pages_per_file)


def page_note_filename(page: RenderedPage, index: int) -> str:
    safe_stem = "".join(
        char if char.isalnum() or char in "-_" else "_"
        for char in page.source_path.stem
    )
    return f"page_{index:04d}_{page.source_role}_{safe_stem}_p{page.page_no:03d}.md"

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "selector", help="atom_id, title substring, or base_name substring."
    )
    parser.add_argument(
        "--manifest",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument("--course-root", default=".", type=Path)
    parser.add_argument(
        "--vision-model",
        default=os.getenv("QWEN_VISION_MODEL", "qwen3.6-35b-a3b"),
        help="Model used for single-page OCR. Default: QWEN_VISION_MODEL or qwen3.6-35b-a3b.",
    )
    parser.add_argument(
        "--synthesis-model",
        default=os.getenv(
            "QWEN_SYNTHESIS_MODEL", os.getenv("QWEN_MODEL", "qwen3.6-plus-2026-04-02")
        ),
        help="Model used for final atom synthesis. Default: QWEN_SYNTHESIS_MODEL/QWEN_MODEL or qwen3.6-plus-2026-04-02.",
    )
    parser.add_argument("--dpi", type=int, default=144)
    parser.add_argument("--max-pages-per-file", type=int)
    parser.add_argument("--max-pages-total", type=int)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--render-only",
        action="store_true",
        help="Render page images and stop before Qwen calls.",
    )
    parser.add_argument(
        "--ocr-only",
        action="store_true",
        help="Run single-page OCR and stop before final atom synthesis.",
    )
    parser.add_argument(
        "--reuse-page-md",
        action="store_true",
        help="Reuse existing per-page Markdown files instead of calling the vision model again.",
    )
    args = parser.parse_args()

    records = read_manifest(args.manifest)
    atom = find_atom(records, args.selector)
    output_md = args.course_root / atom["output_md"]
    cache_dir = (
        args.course_root
        / "国际航运代理理论与实务/整理与索引版本/99_过程缓存"
        / atom["atom_id"]
    )

    print(f"Atom: {atom['atom_id']} {atom['title']}")
    print(f"Chapter: {atom['chapter']}")
    for item in atom["source_files"]:
        print(f"- {item['role']}: {item['path']}")

    if args.dry_run:
        print(f"Would render pages into: {cache_dir}")
        print(f"Would write Markdown to: {output_md}")
        return

    rendered_pages: list[RenderedPage] = []
    for source in atom["source_files"]:
        path = args.course_root / source["path"]
        rendered_pages.extend(
            render_source_file(
                path, source["role"], cache_dir, args.dpi, args.max_pages_per_file
            )
        )

    if args.max_pages_total:
        rendered_pages = rendered_pages[: args.max_pages_total]
    if not rendered_pages:
        raise SystemExit("No pages rendered.")
    if args.render_only:
        print(f"Rendered {len(rendered_pages)} pages into: {cache_dir / 'pages'}")
        return

    client = create_qwen_client()
    raw_dir = cache_dir / "qwen_raw_outputs"
    page_md_dir = cache_dir / "page_md"
    page_md_dir.mkdir(parents=True, exist_ok=True)
    page_notes: list[str] = []
    for index, page in enumerate(rendered_pages, start=1):
        page_md_path = page_md_dir / page_note_filename(page, index)
        if args.reuse_page_md and page_md_path.exists():
            print(
                f"Reusing page OCR {index}/{len(rendered_pages)}: {page_md_path.name}"
            )
            page_note = page_md_path.read_text(encoding="utf-8")
        else:
            print(
                f"Calling Qwen page OCR with {args.vision_model}: {index}/{len(rendered_pages)}: {page.source_role} {page.source_path.name} p.{page.page_no}"
            )
            page_note = call_qwen_page_ocr(
                client, args.vision_model, atom, page, raw_dir, index
            )
            page_md_path.write_text(page_note.rstrip() + "\n", encoding="utf-8")
        page_notes.append(page_note)

    if args.ocr_only:
        print(f"Wrote {len(page_notes)} page OCR Markdown files to: {page_md_dir}")
        return

    print(f"Calling Qwen synthesis with {args.synthesis_model}")
    final_md = call_qwen_synthesis_from_page_md(
        client, args.synthesis_model, atom, page_notes, raw_dir
    )
    output_md.parent.mkdir(parents=True, exist_ok=True)
    output_md.write_text(final_md.rstrip() + "\n", encoding="utf-8")
    print(f"Wrote {output_md}")


if __name__ == "__main__":
    main()
