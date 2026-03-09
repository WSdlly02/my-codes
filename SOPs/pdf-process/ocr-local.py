#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import subprocess
import sys
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser(
        description="批量将目录中的 PDF 转为图片，再调用本地 OCR 输出 Markdown。"
    )
    parser.add_argument("dir_path", help="PDF 文件目录")
    parser.add_argument(
        "-d",
        "--dpi",
        type=int,
        default=300,
        help="PDF 转图片的 DPI，默认 300",
    )
    parser.add_argument(
        "-p",
        "--prompt-style",
        default="text",
        help="OCR 风格，兼容 t/md/f/table/json/desc",
    )
    parser.add_argument(
        "--output-root",
        default=None,
        help="图片和 OCR 结果的根目录，默认使用 PDF 所在目录",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="即使已存在 ocr_results.md 也重新处理",
    )
    args = parser.parse_args()

    input_dir = Path(args.dir_path).expanduser().resolve()
    if not input_dir.is_dir():
        print(f"[Error] {input_dir} 不是有效目录")
        return 1

    base_dir = Path(__file__).resolve().parent.parent
    export_imgs_script = base_dir / "pdf-process" / "export-imgs.py"
    batch_ocr_script = base_dir / "image-process" / "ocr-local.py"

    pdf_files = sorted(path for path in input_dir.iterdir() if path.suffix.lower() == ".pdf")
    if not pdf_files:
        print(f"[Info] 在目录 {input_dir} 中未找到 PDF 文件")
        return 1

    output_root = (
        Path(args.output_root).expanduser().resolve() if args.output_root else input_dir
    )
    output_root.mkdir(parents=True, exist_ok=True)

    print(f"[Batch] 找到 {len(pdf_files)} 个 PDF 文件，准备处理...")
    failures = 0

    for pdf_path in pdf_files:
        output_img_dir = output_root / pdf_path.stem
        ocr_output = output_img_dir / "ocr_results.md"

        if ocr_output.exists() and not args.force:
            print(f"[Skip] 已存在 OCR 结果: {ocr_output}")
            continue

        print(f"\n{'=' * 60}")
        print(f"[Step 1] PDF 转图片: {pdf_path.name}")

        try:
            subprocess.run(
                [
                    sys.executable,
                    str(export_imgs_script),
                    str(pdf_path),
                    "-o",
                    str(output_img_dir),
                    "-d",
                    str(args.dpi),
                ],
                check=True,
                cwd=export_imgs_script.parent,
            )
            print(f"[Step 2] 图片 OCR: {output_img_dir}")
            subprocess.run(
                [
                    sys.executable,
                    str(batch_ocr_script),
                    str(output_img_dir),
                    "-p",
                    args.prompt_style,
                    "-o",
                    str(ocr_output),
                ],
                check=True,
                cwd=batch_ocr_script.parent,
            )
        except subprocess.CalledProcessError as exc:
            failures += 1
            print(f"[Error] 处理失败: {pdf_path.name}, 错误: {exc}")

    print(f"\n{'=' * 60}")
    print("[Done] 所有 PDF 已处理完成。")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
