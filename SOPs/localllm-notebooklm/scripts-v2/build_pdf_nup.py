#!/usr/bin/env python3
"""Merge PDF inputs and place multiple source pages on each output sheet."""

from __future__ import annotations

import argparse
import math
from pathlib import Path

from pypdf import PdfReader, PdfWriter, Transformation


A4_PORTRAIT = (595.276, 841.89)
A4_LANDSCAPE = (841.89, 595.276)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build an A4 n-up PDF from ordered PDF inputs.")
    parser.add_argument("inputs", nargs="+", type=Path, help="Input PDFs in final reading order.")
    parser.add_argument("-o", "--output", required=True, type=Path)
    parser.add_argument("--columns", type=int, default=2)
    parser.add_argument("--rows", type=int, default=2)
    parser.add_argument("--orientation", choices=("landscape", "portrait"), default="landscape")
    parser.add_argument("--margin-pt", type=float, default=12.0)
    parser.add_argument("--gap-pt", type=float, default=8.0)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.columns < 1 or args.rows < 1:
        raise SystemExit("--columns and --rows must be positive")
    if args.margin_pt < 0 or args.gap_pt < 0:
        raise SystemExit("--margin-pt and --gap-pt cannot be negative")

    inputs = [path.expanduser().resolve() for path in args.inputs]
    missing = [path for path in inputs if not path.is_file()]
    if missing:
        raise SystemExit("Missing input PDFs:\n" + "\n".join(str(path) for path in missing))

    output = args.output.expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    sheet_size = A4_LANDSCAPE if args.orientation == "landscape" else A4_PORTRAIT
    per_sheet = args.columns * args.rows

    readers = [PdfReader(str(path)) for path in inputs]
    source_pages = [(path, page) for path, reader in zip(inputs, readers) for page in reader.pages]
    writer = PdfWriter()
    for offset in range(0, len(source_pages), per_sheet):
        sheet = writer.add_blank_page(width=sheet_size[0], height=sheet_size[1])
        for slot, (_, page) in enumerate(source_pages[offset : offset + per_sheet]):
            place_page(sheet, page, slot, args.columns, args.rows, sheet_size, args.margin_pt, args.gap_pt)

    writer.add_metadata(
        {
            "/Title": output.stem,
            "/Subject": f"{args.columns}x{args.rows} n-up, {len(source_pages)} source pages",
        }
    )
    temp_output = output.with_name(f".{output.name}.tmp")
    try:
        with temp_output.open("wb") as handle:
            writer.write(handle)
        temp_output.replace(output)
    finally:
        temp_output.unlink(missing_ok=True)

    expected = math.ceil(len(source_pages) / per_sheet)
    actual = len(PdfReader(str(output)).pages)
    if actual != expected:
        raise RuntimeError(f"output page count mismatch: expected {expected}, got {actual}")
    print(f"Source files: {len(inputs)}")
    print(f"Source pages: {len(source_pages)}")
    print(f"Output pages: {actual}")
    print(f"Wrote: {output}")
    return 0


def place_page(sheet, page, slot: int, columns: int, rows: int, sheet_size, margin: float, gap: float) -> None:
    # Normalize /Rotate before calculating the visible dimensions.
    if page.rotation:
        page.transfer_rotation_to_content()
    source_width = float(page.cropbox.width)
    source_height = float(page.cropbox.height)
    usable_width = sheet_size[0] - 2 * margin - (columns - 1) * gap
    usable_height = sheet_size[1] - 2 * margin - (rows - 1) * gap
    cell_width = usable_width / columns
    cell_height = usable_height / rows
    scale = min(cell_width / source_width, cell_height / source_height)

    column = slot % columns
    row_from_top = slot // columns
    cell_x = margin + column * (cell_width + gap)
    cell_y = sheet_size[1] - margin - (row_from_top + 1) * cell_height - row_from_top * gap
    x = cell_x + (cell_width - source_width * scale) / 2
    y = cell_y + (cell_height - source_height * scale) / 2
    transform = Transformation().scale(scale).translate(x, y)
    sheet.merge_transformed_page(page, transform, over=True, expand=False)


if __name__ == "__main__":
    raise SystemExit(main())
