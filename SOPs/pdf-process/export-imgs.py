#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse
import fitz  # PyMuPDF


def convert_pdf_to_images(pdf_path, output_dir, dpi=300, fmt="png"):
    """
    将 PDF 转换为多张图片
    """
    if not os.path.exists(pdf_path):
        print(f"[Error] 文件不存在: {pdf_path}")
        return

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"[Info] 创建输出目录: {output_dir}")

    # 打开 PDF 文件
    pdf_document = fitz.open(pdf_path)
    total_pages = len(pdf_document)
    print(f"[Info] 正在处理 {pdf_path}，共 {total_pages} 页...")

    # 设置缩放比例 (DPI / 72)
    zoom = dpi / 72
    matrix = fitz.Matrix(zoom, zoom)

    pdf_name = os.path.splitext(os.path.basename(pdf_path))[0]

    for page_index in range(total_pages):
        page = pdf_document.load_page(page_index)
        pix = page.get_pixmap(matrix=matrix)

        # 生成文件名，例如: document_page_001.png
        output_filename = f"{pdf_name}_page_{page_index + 1:03d}.{fmt}"
        output_path = os.path.join(output_dir, output_filename)

        pix.save(output_path)
        print(f"  [{page_index + 1}/{total_pages}] 已保存: {output_filename}")

    pdf_document.close()
    print(f"\n[Done] 转换完成，图片保存在: {output_dir}")


def main():
    parser = argparse.ArgumentParser(description="将 PDF 文件转换为多张图片")
    parser.add_argument("pdf_path", help="PDF 文件的路径")
    parser.add_argument(
        "-o", "--output", help="输出目录 (默认: 与 PDF 同名的文件夹)", default=None
    )
    parser.add_argument(
        "-d", "--dpi", type=int, default=300, help="输出图片的 DPI (默认: 300)"
    )
    parser.add_argument(
        "-f",
        "--format",
        default="png",
        choices=["png", "jpg", "jpeg"],
        help="图片格式 (默认: png)",
    )

    args = parser.parse_args()

    # 如果未指定输出目录，则使用 PDF 文件名作为目录名
    if args.output is None:
        pdf_dir = os.path.dirname(os.path.abspath(args.pdf_path))
        pdf_name = os.path.splitext(os.path.basename(args.pdf_path))[0]
        args.output = os.path.join(pdf_dir, pdf_name)

    convert_pdf_to_images(args.pdf_path, args.output, args.dpi, args.format)


if __name__ == "__main__":
    main()
