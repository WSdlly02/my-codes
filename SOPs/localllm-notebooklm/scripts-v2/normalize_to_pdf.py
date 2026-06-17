#!/usr/bin/env python3
"""Normalize course documents to PDFs for the v2 OCR workflow.

This script is intentionally course-agnostic. It scans one course directory,
skips generated/cache directories, copies existing PDFs into a normalized PDF
folder, and converts office documents to PDF with LibreOffice.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
from dataclasses import asdict, dataclass
from pathlib import Path


DEFAULT_SKIP_DIRS = {
    ".git",
    ".obsidian",
    ".trash",
    "__pycache__",
    "整理与索引版本",
}
PDF_SUFFIXES = {".pdf"}
OFFICE_SUFFIXES = {".doc", ".docx", ".ppt", ".pptx"}
SUPPORTED_SUFFIXES = PDF_SUFFIXES | OFFICE_SUFFIXES


@dataclass(frozen=True)
class NormalizedRecord:
    source_path: str
    source_suffix: str
    pdf_id: str
    pdf_name: str
    pdf_path: str
    action: str
    status: str
    error: str | None = None


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Normalize PDFs/DOCX/PPTX files into a v2 OCR PDF cache."
    )
    parser.add_argument("course_dir", type=Path, help="Course root directory.")
    parser.add_argument(
        "--output-dir",
        type=Path,
        help=(
            "Normalized PDF output directory. Defaults to "
            "<course_dir>/整理与索引版本/99_过程缓存/pdf_normalized."
        ),
    )
    parser.add_argument(
        "--manifest-out",
        type=Path,
        help=(
            "JSONL audit output. Defaults to "
            "<course_dir>/整理与索引版本/99_过程缓存/normalized_files.jsonl."
        ),
    )
    parser.add_argument(
        "--profile-dir",
        type=Path,
        help=(
            "LibreOffice profile directory. Defaults to "
            "<course_dir>/整理与索引版本/99_过程缓存/libreoffice_profiles."
        ),
    )
    parser.add_argument(
        "--keep-profile-dir",
        action="store_true",
        help="Keep LibreOffice profile directory after normalization.",
    )
    parser.add_argument(
        "--include-hidden",
        action="store_true",
        help="Include hidden files and directories except explicit skip dirs.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Regenerate normalized PDFs even when the target PDF already exists.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print planned work and write no files.",
    )
    parser.add_argument(
        "--libreoffice",
        default="libreoffice",
        help="LibreOffice executable name or path. Default: libreoffice.",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=180,
        help="LibreOffice conversion timeout in seconds. Default: 180.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    course_dir = args.course_dir.resolve()
    if not course_dir.is_dir():
        print(f"ERROR: course_dir is not a directory: {course_dir}", file=sys.stderr)
        return 2

    output_dir = (
        args.output_dir.resolve()
        if args.output_dir
        else course_dir / "整理与索引版本" / "99_过程缓存" / "pdf_normalized"
    )
    manifest_out = (
        args.manifest_out.resolve()
        if args.manifest_out
        else course_dir / "整理与索引版本" / "99_过程缓存" / "normalized_files.jsonl"
    )
    profile_dir = (
        args.profile_dir.resolve()
        if args.profile_dir
        else course_dir / "整理与索引版本" / "99_过程缓存" / "libreoffice_profiles"
    )

    sources = list(iter_source_files(course_dir, include_hidden=args.include_hidden))
    print(f"Found {len(sources)} supported source files under {course_dir}")

    if args.dry_run:
        for index, source in enumerate(sources, start=1):
            pdf_id = build_pdf_id(course_dir, source, index)
            print(f"[dry-run] {source} -> {output_dir / (pdf_id + '.pdf')}")
        return 0

    output_dir.mkdir(parents=True, exist_ok=True)
    manifest_out.parent.mkdir(parents=True, exist_ok=True)
    profile_dir.mkdir(parents=True, exist_ok=True)

    records: list[NormalizedRecord] = []
    for index, source in enumerate(sources, start=1):
        print(f"[{index}/{len(sources)}] {source.relative_to(course_dir)}")
        record = normalize_one(
            course_dir=course_dir,
            source=source,
            index=index,
            output_dir=output_dir,
            profile_dir=profile_dir,
            libreoffice=args.libreoffice,
            timeout=args.timeout,
            overwrite=args.overwrite,
        )
        records.append(record)
        if record.status == "ok":
            print(f"  {record.action}: {record.pdf_path}")
        else:
            print(f"  ERROR: {record.error}", file=sys.stderr)

    with manifest_out.open("w", encoding="utf-8") as fh:
        for record in records:
            fh.write(json.dumps(asdict(record), ensure_ascii=False) + "\n")

    ok = sum(1 for record in records if record.status == "ok")
    failed = len(records) - ok
    if not args.keep_profile_dir:
        remove_path(profile_dir)
        print(f"Removed LibreOffice profile dir: {profile_dir}")
    print(f"Done. ok={ok}, failed={failed}, manifest={manifest_out}")
    return 1 if failed else 0


def iter_source_files(course_dir: Path, include_hidden: bool) -> list[Path]:
    results: list[Path] = []

    for path in sorted(course_dir.rglob("*"), key=lambda p: str(p)):
        if should_skip_path(course_dir, path, include_hidden=include_hidden):
            continue
        if path.is_file() and path.suffix.lower() in SUPPORTED_SUFFIXES:
            results.append(path)

    return results


def should_skip_path(course_dir: Path, path: Path, include_hidden: bool) -> bool:
    try:
        relative = path.relative_to(course_dir)
    except ValueError:
        return True

    for part in relative.parts:
        if part in DEFAULT_SKIP_DIRS:
            return True
        if not include_hidden and part.startswith("."):
            return True
    return False


def normalize_one(
    *,
    course_dir: Path,
    source: Path,
    index: int,
    output_dir: Path,
    profile_dir: Path,
    libreoffice: str,
    timeout: int,
    overwrite: bool,
) -> NormalizedRecord:
    pdf_id = build_pdf_id(course_dir, source, index)
    pdf_name = source.stem
    target_pdf = output_dir / f"{pdf_id}.pdf"
    suffix = source.suffix.lower()

    try:
        if target_pdf.exists() and not overwrite:
            return build_record(course_dir, source, target_pdf, pdf_id, pdf_name, "skip")
        if suffix in PDF_SUFFIXES:
            shutil.copy2(source, target_pdf)
            return build_record(course_dir, source, target_pdf, pdf_id, pdf_name, "copy")
        if suffix in OFFICE_SUFFIXES:
            convert_office_to_pdf(
                source=source,
                target_pdf=target_pdf,
                profile_dir=profile_dir / pdf_id,
                libreoffice=libreoffice,
                timeout=timeout,
            )
            return build_record(course_dir, source, target_pdf, pdf_id, pdf_name, "convert")
        raise ValueError(f"unsupported suffix: {source.suffix}")
    except Exception as exc:  # noqa: BLE001 - keep batch processing after one file fails.
        return NormalizedRecord(
            source_path=str(source.relative_to(course_dir)),
            source_suffix=suffix,
            pdf_id=pdf_id,
            pdf_name=pdf_name,
            pdf_path=str(target_pdf),
            action="error",
            status="failed",
            error=str(exc),
        )


def convert_office_to_pdf(
    *,
    source: Path,
    target_pdf: Path,
    profile_dir: Path,
    libreoffice: str,
    timeout: int,
) -> None:
    if not shutil.which(libreoffice) and not Path(libreoffice).exists():
        raise RuntimeError(f"LibreOffice executable not found: {libreoffice}")

    profile_dir.mkdir(parents=True, exist_ok=True)
    out_dir = target_pdf.parent
    produced_pdf = out_dir / f"{source.stem}.pdf"
    if produced_pdf != target_pdf:
        remove_path(produced_pdf)

    command = [
        libreoffice,
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
    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False,
    )
    if result.returncode != 0:
        details = format_libreoffice_error(command, result)
        raise RuntimeError(details)

    if not produced_pdf.exists():
        raise RuntimeError(f"LibreOffice did not produce expected PDF: {produced_pdf}")

    shutil.move(str(produced_pdf), target_pdf)


def build_record(
    course_dir: Path,
    source: Path,
    target_pdf: Path,
    pdf_id: str,
    pdf_name: str,
    action: str,
) -> NormalizedRecord:
    return NormalizedRecord(
        source_path=str(source.relative_to(course_dir)),
        source_suffix=source.suffix.lower(),
        pdf_id=pdf_id,
        pdf_name=pdf_name,
        pdf_path=str(target_pdf),
        action=action,
        status="ok",
        error=None,
    )


def build_pdf_id(course_dir: Path, source: Path, index: int) -> str:
    relative = source.relative_to(course_dir).as_posix()
    digest = hashlib.sha1(relative.encode("utf-8")).hexdigest()[:10]
    return f"P{index:06d}-{digest}"


def format_libreoffice_error(command: list[str], result: subprocess.CompletedProcess[str]) -> str:
    stdout = result.stdout.strip()
    stderr = result.stderr.strip()
    parts = [f"LibreOffice failed with code {result.returncode}"]
    if stderr:
        parts.append(f"stderr={stderr[:500]}")
    if stdout:
        parts.append(f"stdout={stdout[:500]}")
    parts.append(f"command={' '.join(command)}")
    return "; ".join(parts)


def remove_path(path: Path) -> None:
    if path.is_dir():
        shutil.rmtree(path)
    elif path.exists():
        path.unlink()


if __name__ == "__main__":
    raise SystemExit(main())
