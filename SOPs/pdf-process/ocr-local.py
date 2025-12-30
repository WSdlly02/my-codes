#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(
        description="批量将文件夹下的 PDF 转换为图片并进行 OCR 识别汇总"
    )
    parser.add_argument("dir_path", help="包含 PDF 文件的文件夹路径")
    parser.add_argument(
        "-d", "--dpi", type=int, default=300, help="PDF 转图片的 DPI (默认: 300)"
    )
    parser.add_argument(
        "-p", "--prompt-style", default="t", help="OCR 识别风格 (默认: t)"
    )

    args = parser.parse_args()

    if not os.path.isdir(args.dir_path):
        print(f"[Error] {args.dir_path} 不是一个有效的目录")
        return

    # 获取当前脚本所在目录，以便定位其他脚本
    # 假设脚本都在各自的 SOPs 子目录下，或者用户在特定目录下运行
    # 根据 workspace 结构：
    # SOPs/pdf-process/export-imgs.py
    # SOPs/image-process/ocr-local.py

    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    export_imgs_script = os.path.join(base_dir, "pdf-process", "export-imgs.py")
    batch_ocr_script = os.path.join(base_dir, "image-process", "ocr-local.py")
    # 1. 查找所有 PDF 文件
    pdf_files = [f for f in os.listdir(args.dir_path) if f.lower().endswith(".pdf")]
    pdf_files.sort()

    if not pdf_files:
        print(f"[Info] 在目录 {args.dir_path} 中未找到 PDF 文件")
        return

    print(f"[Batch] 找到 {len(pdf_files)} 个 PDF 文件，准备处理...")

    for pdf_file in pdf_files:
        pdf_path = os.path.join(args.dir_path, pdf_file)
        pdf_name = os.path.splitext(pdf_file)[0]
        output_img_dir = os.path.join(args.dir_path, pdf_name)

        print(f"\n{'='*60}")
        print(f"[Step 1] 正在转换 PDF 为图片: {pdf_file}")

        # 调用 pdf-to-imgs.py
        try:
            subprocess.run(
                [
                    sys.executable,
                    export_imgs_script,
                    pdf_path,
                    "-o",
                    output_img_dir,
                    "-d",
                    str(args.dpi),
                ],
                check=True,
                cwd=os.path.dirname(export_imgs_script),
            )
        except subprocess.CalledProcessError as e:
            print(f"[Error] PDF 转换失败: {pdf_file}, 错误: {e}")
            continue

        print(f"[Step 2] 正在对图片进行批量 OCR: {output_img_dir}")

        # 调用 ocr-local.py
        # ocr-local.py 默认在图片目录下生成 ocr_results.md
        try:
            subprocess.run(
                [
                    sys.executable,
                    batch_ocr_script,
                    output_img_dir,
                    "-p",
                    args.prompt_style,
                ],
                check=True,
                cwd=os.path.dirname(batch_ocr_script),
            )
        except subprocess.CalledProcessError as e:
            print(f"[Error] OCR 批量处理失败: {output_img_dir}, 错误: {e}")
            continue

    print(f"\n{'='*60}")
    print(f"[Done] 所有 PDF 已处理完成。")


if __name__ == "__main__":
    main()
