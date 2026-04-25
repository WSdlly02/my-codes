#!/usr/bin/env python3
"""Convert one atom using PaddleOCR layout parsing plus Qwen synthesis."""

from __future__ import annotations

import argparse
import json
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from urllib.parse import urlparse

import requests
from dotenv import load_dotenv

from convert_atom import (
    convert_docx_to_pdf,
    find_atom,
    read_manifest,
)
from llmcall import (
    call_paddle_layout,
    call_qwen_synthesis_from_paddle_md,
    create_qwen_client,
)


DEFAULT_SYNTHESIS_MODEL = "qwen3.6-plus-2026-04-02"


@dataclass(frozen=True)
class SourcePdf:
    role: str
    source_path: Path
    pdf_path: Path


def safe_stem(path: Path) -> str:
    return "".join(
        char if char.isalnum() or char in "-_" else "_" for char in path.stem
    )


def page_note_filename(source: SourcePdf, index: int, local_page_no: int) -> str:
    return f"page_{index:04d}_{source.role}_{safe_stem(source.source_path)}_p{local_page_no:03d}.md"


def source_to_pdf(source: Path, role: str, work_dir: Path) -> SourcePdf:
    suffix = source.suffix.lower()
    if suffix == ".pdf":
        pdf = source
    elif suffix == ".docx":
        pdf = convert_docx_to_pdf(source, work_dir)
    else:
        raise ValueError(f"Unsupported source file: {source}")
    return SourcePdf(role=role, source_path=source, pdf_path=pdf)


def download_url(url: str, output_path: Path, timeout: int) -> None:
    response = requests.get(url, timeout=timeout)
    response.raise_for_status()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_bytes(response.content)


def download_paddle_assets(
    result: dict, source: SourcePdf, output_dir: Path, timeout: int
) -> None:
    for result_index, item in enumerate(
        result.get("layoutParsingResults", []) or [], start=1
    ):
        markdown_images = item.get("markdown", {}).get("images", {}) or {}
        for image_path, url in markdown_images.items():
            safe_rel = Path(str(image_path))
            if safe_rel.is_absolute() or ".." in safe_rel.parts:
                safe_rel = Path(urlparse(str(url)).path).name
            output_path = (
                output_dir / source.role / f"result_{result_index:03d}" / safe_rel
            )
            download_url(str(url), output_path, timeout)

        output_images = item.get("outputImages", {}) or {}
        for image_name, url in output_images.items():
            output_path = (
                output_dir
                / source.role
                / f"{source.pdf_path.stem}_{image_name}_{result_index:03d}.jpg"
            )
            download_url(str(url), output_path, timeout)


def format_paddle_page_md(
    atom: dict, source: SourcePdf, local_page_no: int, markdown: str
) -> str:
    body = markdown.strip() or "（PaddleOCR 未返回可用 Markdown 文本）"
    return "\n".join(
        [
            "---",
            f"source_role: {source.role}",
            f"source_file: {source.source_path.name}",
            f"page_no: {local_page_no}",
            "ocr_provider: paddle_layout_parsing",
            f"atom_id: {atom['atom_id']}",
            "---",
            "",
            "## OCR正文",
            "",
            body,
            "",
        ]
    )


def write_paddle_pages(
    result: dict,
    atom: dict,
    source: SourcePdf,
    page_md_dir: Path,
    raw_dir: Path,
    assets_dir: Path,
    global_start_index: int,
    download_assets: bool,
    timeout: int,
) -> list[str]:
    raw_dir.mkdir(parents=True, exist_ok=True)
    page_md_dir.mkdir(parents=True, exist_ok=True)
    (
        raw_dir
        / f"{global_start_index:04d}_{source.role}_{safe_stem(source.source_path)}.json"
    ).write_text(
        json.dumps(result, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )
    if download_assets:
        download_paddle_assets(result, source, assets_dir, timeout)

    page_notes: list[str] = []
    items = result.get("layoutParsingResults", []) or []
    if not items:
        items = [{"markdown": {"text": ""}}]
    for local_page_no, item in enumerate(items, start=1):
        global_index = global_start_index + local_page_no - 1
        markdown = item.get("markdown", {}).get("text", "") or ""
        page_note = format_paddle_page_md(atom, source, local_page_no, markdown)
        page_md_path = page_md_dir / page_note_filename(
            source, global_index, local_page_no
        )
        page_md_path.write_text(page_note.rstrip() + "\n", encoding="utf-8")
        page_notes.append(page_note)
    return page_notes


def read_existing_page_notes(page_md_dir: Path) -> list[str]:
    return [
        path.read_text(encoding="utf-8")
        for path in sorted(page_md_dir.glob("page_*.md"))
    ]


def main() -> None:
    load_dotenv()
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
        "--output-md",
        type=Path,
        help="Optional output Markdown path. Defaults to the manifest output_md path.",
    )
    parser.add_argument("--pp-api-key", default=os.getenv("PP_API_KEY"))
    parser.add_argument("--timeout", type=int, default=300)
    parser.add_argument("--retries", type=int, default=2)
    parser.add_argument(
        "--synthesis-model",
        default=os.getenv(
            "QWEN_SYNTHESIS_MODEL", os.getenv("QWEN_MODEL", DEFAULT_SYNTHESIS_MODEL)
        ),
        help="Model used for final atom synthesis.",
    )
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--ocr-only",
        action="store_true",
        help="Run PaddleOCR and stop before final synthesis.",
    )
    parser.add_argument(
        "--reuse-page-md",
        action="store_true",
        help="Reuse existing v2 Paddle page Markdown instead of calling PaddleOCR again.",
    )
    parser.add_argument(
        "--download-assets",
        action="store_true",
        help="Download images referenced by PaddleOCR markdown and outputImages.",
    )
    args = parser.parse_args()

    records = read_manifest(args.manifest)
    atom = find_atom(records, args.selector)
    output_md = (
        args.output_md if args.output_md else args.course_root / atom["output_md"]
    )
    cache_dir = (
        args.course_root
        / "国际航运代理理论与实务/整理与索引版本/99_过程缓存"
        / atom["atom_id"]
    )
    page_md_dir = cache_dir / "page_md_v2_paddle"
    raw_dir = cache_dir / "paddle_raw_outputs"
    qwen_raw_dir = cache_dir / "qwen_raw_outputs_v2"
    assets_dir = cache_dir / "paddle_assets"

    print(f"Atom: {atom['atom_id']} {atom['title']}")
    print(f"Chapter: {atom['chapter']}")
    for item in atom["source_files"]:
        print(f"- {item['role']}: {item['path']}")
    print(f"Paddle page_md dir: {page_md_dir}")
    print(f"Output Markdown: {output_md}")

    if args.dry_run:
        return
    if not args.pp_api_key and not args.reuse_page_md:
        raise RuntimeError(
            "Set PP_API_KEY or pass --pp-api-key before calling PaddleOCR."
        )

    if args.reuse_page_md and page_md_dir.exists():
        page_notes = read_existing_page_notes(page_md_dir)
        if not page_notes:
            raise RuntimeError(
                f"No existing v2 Paddle page Markdown found in {page_md_dir}"
            )
        print(f"Reusing {len(page_notes)} v2 Paddle page Markdown file(s).")
    else:
        page_md_dir.mkdir(parents=True, exist_ok=True)
        page_notes = []
        global_index = 1
        for source_record in atom["source_files"]:
            source_path = args.course_root / source_record["path"]
            source_pdf = source_to_pdf(source_path, source_record["role"], cache_dir)
            print(
                f"Calling PaddleOCR layout parsing: {source_record['role']} "
                f"{source_pdf.pdf_path.name}",
                flush=True,
            )
            result = call_paddle_layout(
                args.pp_api_key,
                source_pdf.pdf_path,
                file_type=0,
                timeout=args.timeout,
                retries=args.retries,
            )
            source_notes = write_paddle_pages(
                result,
                atom,
                source_pdf,
                page_md_dir,
                raw_dir,
                assets_dir,
                global_index,
                args.download_assets,
                args.timeout,
            )
            page_notes.extend(source_notes)
            global_index += len(source_notes)

    if args.ocr_only:
        print(
            f"Wrote {len(page_notes)} v2 Paddle page Markdown files to: {page_md_dir}"
        )
        return

    client = create_qwen_client()
    print(f"Calling Qwen synthesis with {args.synthesis_model}")
    final_md = call_qwen_synthesis_from_paddle_md(
        client, args.synthesis_model, atom, page_notes, qwen_raw_dir
    )
    output_md.parent.mkdir(parents=True, exist_ok=True)
    output_md.write_text(final_md.rstrip() + "\n", encoding="utf-8")
    print(f"Wrote {output_md}")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        raise
    except Exception as exc:  # noqa: BLE001
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
