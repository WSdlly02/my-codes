#!/usr/bin/env python3
"""Render one Markdown document to a print-oriented PDF.

Pandoc parses Markdown.  XeLaTeX is preferred when available because it gives
stable pagination and native page numbers; Chrome is the lightweight fallback.
"""

from __future__ import annotations

import argparse
import io
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


DEFAULT_CHROME_CANDIDATES = (
    "google-chrome",
    "google-chrome-stable",
    "chromium",
    "chromium-browser",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Render Markdown as an A4 PDF.")
    parser.add_argument("input_md", type=Path, help="Input Markdown file.")
    parser.add_argument("-o", "--output", type=Path, help="Output PDF; defaults beside the input file.")
    parser.add_argument(
        "--engine",
        choices=("auto", "xelatex", "chrome"),
        default="auto",
        help="PDF engine. Auto prefers XeLaTeX and falls back to Chrome.",
    )
    parser.add_argument("--pandoc", default="pandoc", help="Pandoc executable.")
    parser.add_argument("--chrome", help="Chrome/Chromium executable for the Chrome engine.")
    parser.add_argument("--font", default="Noto Serif CJK SC", help="Main Chinese font.")
    parser.add_argument("--sans-font", default="Noto Sans CJK SC", help="Heading/sans-serif font.")
    parser.add_argument("--margin-mm", type=float, default=16.0, help="A4 page margin in millimetres.")
    parser.add_argument("--toc", action="store_true", help="Generate a table of contents.")
    parser.add_argument("--number-sections", action="store_true", help="Number document headings.")
    parser.add_argument("--page-numbers", action="store_true", help="Add clean continuous page numbers at bottom centre.")
    parser.add_argument(
        "--chrome-headers",
        action="store_true",
        help="Keep Chrome's date/title/URL/page-count headers and footers.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    source = args.input_md.expanduser().resolve()
    if not source.is_file():
        raise SystemExit(f"Markdown file not found: {source}")
    if args.margin_mm <= 0:
        raise SystemExit("--margin-mm must be positive")

    output = (args.output or source.with_suffix(".pdf")).expanduser().resolve()
    if output.suffix.lower() != ".pdf":
        raise SystemExit(f"Output must use the .pdf suffix: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)

    pandoc = require_executable(args.pandoc, "Pandoc")
    engine_name, engine_path = choose_engine(args.engine, args.chrome)
    with tempfile.TemporaryDirectory(prefix="md-to-pdf-", dir=output.parent) as temp_name:
        temp_dir = Path(temp_name)
        temp_pdf = temp_dir / output.name
        if engine_name == "xelatex":
            render_xelatex(source, temp_pdf, pandoc, engine_path, args)
        else:
            render_chrome(source, temp_pdf, temp_dir, pandoc, engine_path, args)
        if args.page_numbers:
            numbered_pdf = temp_dir / f"numbered-{output.name}"
            stamp_page_numbers(temp_pdf, numbered_pdf)
            numbered_pdf.replace(temp_pdf)
        validate_pdf(temp_pdf)
        temp_pdf.replace(output)

    print(f"Engine: {engine_name} ({engine_path})")
    print(f"Wrote: {output}")
    return 0


def require_executable(value: str, label: str) -> str:
    path = shutil.which(value)
    if path:
        return path
    if value == "pandoc":
        try:
            import pypandoc

            bundled = pypandoc.get_pandoc_path()
            if bundled and Path(bundled).is_file():
                return bundled
        except (ImportError, OSError):
            pass
    raise SystemExit(
        f"{label} executable not found: {value}\n"
        "Install it or run through Nix, for example: "
        "nix shell nixpkgs#pandoc -c python scripts-v2/render_markdown_pdf.py <file.md>"
    )


def choose_engine(requested: str, chrome_arg: str | None) -> tuple[str, str]:
    if requested == "xelatex":
        return "xelatex", require_executable("xelatex", "XeLaTeX")
    if requested == "chrome":
        return "chrome", find_chrome(chrome_arg)
    xelatex = shutil.which("xelatex")
    if xelatex:
        return "xelatex", xelatex
    return "chrome", find_chrome(chrome_arg)


def find_chrome(explicit: str | None) -> str:
    if explicit:
        return require_executable(explicit, "Chrome")
    for candidate in DEFAULT_CHROME_CANDIDATES:
        path = shutil.which(candidate)
        if path:
            return path
    raise SystemExit("Neither XeLaTeX nor Chrome/Chromium is available")


def common_pandoc_args(source: Path, args: argparse.Namespace) -> list[str]:
    command = [
        "--from=gfm+tex_math_dollars",
        f"--resource-path={source.parent}",
    ]
    if args.toc:
        command.append("--toc")
    if args.number_sections:
        command.append("--number-sections")
    return command


def render_xelatex(source: Path, output: Path, pandoc: str, xelatex: str, args: argparse.Namespace) -> None:
    margin = f"margin={args.margin_mm:g}mm"
    command = [
        pandoc,
        str(source),
        *common_pandoc_args(source, args),
        "--standalone",
        f"--pdf-engine={xelatex}",
        f"--variable=CJKmainfont:{args.font}",
        f"--variable=mainfont:{args.font}",
        f"--variable=sansfont:{args.sans_font}",
        "--variable=geometry:a4paper",
        f"--variable=geometry:{margin}",
        "--variable=colorlinks:true",
        "--variable=linkcolor:black",
        "--variable=urlcolor:blue",
        "--output",
        str(output),
    ]
    run(command, source.parent)


def render_chrome(
    source: Path,
    output: Path,
    temp_dir: Path,
    pandoc: str,
    chrome: str,
    args: argparse.Namespace,
) -> None:
    css = temp_dir / "print.css"
    html = temp_dir / "document.html"
    css.write_text(print_css(args), encoding="utf-8")
    pandoc_command = [
        pandoc,
        str(source),
        *common_pandoc_args(source, args),
        "--standalone",
        f"--metadata=pagetitle:{source.stem}",
        "--embed-resources",
        "--mathml",
        "--css",
        str(css),
        "--output",
        str(html),
    ]
    run(pandoc_command, source.parent)

    chrome_command = [
        chrome,
        "--headless",
        "--disable-gpu",
        "--disable-dev-shm-usage",
        f"--print-to-pdf={output}",
    ]
    if not args.chrome_headers:
        chrome_command.append("--no-pdf-header-footer")
    chrome_command.append(html.as_uri())
    run(chrome_command, source.parent)


def print_css(args: argparse.Namespace) -> str:
    margin = f"{args.margin_mm:g}mm"
    return f"""
@page {{ size: A4; margin: {margin}; }}
html {{ font-size: 10.5pt; }}
body {{
  color: #111;
  background: white;
  font-family: "{args.font}", serif;
  line-height: 1.58;
  overflow-wrap: anywhere;
}}
h1, h2, h3, h4, h5, h6 {{
  color: #111;
  font-family: "{args.sans_font}", sans-serif;
  line-height: 1.3;
  break-after: avoid-page;
}}
h1 {{ font-size: 20pt; margin: 0 0 12pt; }}
h2 {{ font-size: 15pt; margin: 18pt 0 7pt; border-bottom: 0.5pt solid #aaa; padding-bottom: 3pt; }}
h3 {{ font-size: 12.5pt; margin: 14pt 0 5pt; }}
p, ul, ol, blockquote {{ margin-top: 5pt; margin-bottom: 5pt; }}
a {{ color: #0645ad; text-decoration: none; }}
table {{ width: 100%; border-collapse: collapse; margin: 8pt 0; font-size: 8.8pt; }}
thead {{ display: table-header-group; }}
tr {{ break-inside: avoid-page; }}
th, td {{ border: 0.5pt solid #777; padding: 3.5pt 4.5pt; vertical-align: top; }}
th {{ background: #eee; font-family: "{args.sans_font}", sans-serif; }}
pre, code {{ font-family: "Maple Mono NF CN", "Noto Sans Mono CJK SC", monospace; }}
code {{ font-size: 0.9em; }}
pre {{ white-space: pre-wrap; background: #f5f5f5; border: 0.5pt solid #ccc; padding: 7pt; break-inside: avoid-page; }}
blockquote {{ border-left: 3pt solid #999; padding-left: 9pt; color: #333; }}
img, svg {{ max-width: 100%; height: auto; break-inside: avoid-page; }}
hr {{ border: 0; border-top: 0.6pt solid #999; margin: 14pt 0; }}
.page-break {{ break-before: page; page-break-before: always; }}
""".strip() + "\n"


def run(command: list[str], cwd: Path) -> None:
    print("+ " + " ".join(command))
    result = subprocess.run(command, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode != 0:
        if result.stdout:
            print(result.stdout, file=sys.stderr, end="")
        raise RuntimeError(f"command failed with exit code {result.returncode}: {command[0]}")


def validate_pdf(path: Path) -> None:
    if not path.is_file() or path.stat().st_size < 100:
        raise RuntimeError(f"PDF was not created or is empty: {path}")
    with path.open("rb") as handle:
        if handle.read(5) != b"%PDF-":
            raise RuntimeError(f"output is not a PDF: {path}")


def stamp_page_numbers(source: Path, output: Path) -> None:
    try:
        from pypdf import PdfReader, PdfWriter
        from reportlab.pdfgen import canvas
    except ImportError as exc:
        raise RuntimeError("--page-numbers requires pypdf and reportlab") from exc

    reader = PdfReader(str(source))
    writer = PdfWriter()
    for number, page in enumerate(reader.pages, 1):
        width = float(page.mediabox.width)
        height = float(page.mediabox.height)
        overlay_data = io.BytesIO()
        overlay = canvas.Canvas(overlay_data, pagesize=(width, height))
        overlay.setFont("Helvetica", 8)
        overlay.setFillGray(0.35)
        overlay.drawCentredString(width / 2, 11, str(number))
        overlay.save()
        overlay_data.seek(0)
        page.merge_page(PdfReader(overlay_data).pages[0])
        writer.add_page(page)
    with output.open("wb") as handle:
        writer.write(handle)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        raise SystemExit(130)
