#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
from pathlib import Path

import fitz


def convert_pdf_to_images(pdf_path: Path, output_dir: Path, dpi: int = 300, fmt: str = "png"):
    if not pdf_path.is_file():
        raise FileNotFoundError(f"文件不存在: {pdf_path}")
    if dpi <= 0:
        raise ValueError("DPI 必须大于 0")

    output_dir.mkdir(parents=True, exist_ok=True)

    with fitz.open(pdf_path) as pdf_document:
        total_pages = len(pdf_document)
        print(f"[Info] 正在处理 {pdf_path}，共 {total_pages} 页...")

        zoom = dpi / 72
        matrix = fitz.Matrix(zoom, zoom)
        pdf_name = pdf_path.stem

        for page_index in range(total_pages):
            page = pdf_document.load_page(page_index)
            pix = page.get_pixmap(matrix=matrix, alpha=False)
            output_filename = output_dir / f"{pdf_name}_page_{page_index + 1:03d}.{fmt}"
            pix.save(output_filename)
            print(f"  [{page_index + 1}/{total_pages}] 已保存: {output_filename.name}")

    print(f"[Done] 转换完成，图片保存在: {output_dir}")


def main() -> int:
    parser = argparse.ArgumentParser(description="将 PDF 文件导出为图片序列。")
    parser.add_argument("pdf_path", help="PDF 文件路径")
    parser.add_argument(
        "-o",
        "--output",
        help="输出目录，默认使用 PDF 同名目录",
        default=None,
    )
    parser.add_argument(
        "-d",
        "--dpi",
        type=int,
        default=300,
        help="输出图片 DPI，默认 300",
    )
    parser.add_argument(
        "-f",
        "--format",
        default="png",
        choices=["png", "jpg", "jpeg"],
        help="图片格式，默认 png",
    )
    args = parser.parse_args()

    pdf_path = Path(args.pdf_path).expanduser().resolve()
    output_dir = (
        Path(args.output).expanduser().resolve()
        if args.output
        else pdf_path.parent / pdf_path.stem
    )

    try:
        convert_pdf_to_images(pdf_path, output_dir, args.dpi, args.format)
    except Exception as exc:
        print(f"[Error] {exc}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
