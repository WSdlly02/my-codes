#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import base64
from pathlib import Path
from typing import Iterable

from ocrcore import DEFAULT_MODEL, DEFAULT_PROMPT_MAP, generate_ocr_stream_local

SUPPORTED_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".gif", ".tif", ".tiff"}


def encode_image_from_path(image_path: Path) -> str:
    with image_path.open("rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


def iter_image_files(dir_path: Path, recursive: bool) -> Iterable[Path]:
    pattern = "**/*" if recursive else "*"
    return sorted(
        path
        for path in dir_path.glob(pattern)
        if path.is_file() and path.suffix.lower() in SUPPORTED_EXTENSIONS
    )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="批量对目录中的图片执行本地 OCR，并汇总为 Markdown 文档。"
    )
    parser.add_argument("dir_path", help="包含图片的目录路径")
    parser.add_argument(
        "-o",
        "--output",
        default="ocr_results.md",
        help="输出 Markdown 文件名或路径，默认在输入目录下生成 ocr_results.md",
    )
    parser.add_argument(
        "-p",
        "--prompt-style",
        choices=sorted(DEFAULT_PROMPT_MAP),
        default="text",
        help="OCR 风格，兼容旧缩写 t/md/f/table/json/desc",
    )
    parser.add_argument(
        "-m",
        "--model",
        default=DEFAULT_MODEL,
        help=f"Ollama 模型名，默认: {DEFAULT_MODEL}",
    )
    parser.add_argument(
        "--recursive",
        action="store_true",
        help="递归扫描子目录中的图片",
    )
    parser.add_argument(
        "--ollama-url",
        default="http://localhost:11434/api/generate",
        help="Ollama generate API 地址",
    )
    args = parser.parse_args()

    input_dir = Path(args.dir_path).expanduser().resolve()
    if not input_dir.is_dir():
        print(f"[Batch OCR] 错误：{input_dir} 不是有效目录")
        return 1

    output_path = Path(args.output).expanduser()
    if not output_path.is_absolute():
        output_path = input_dir / output_path
    output_path.parent.mkdir(parents=True, exist_ok=True)

    files = list(iter_image_files(input_dir, args.recursive))
    if output_path in files:
        files.remove(output_path)

    if not files:
        print(f"[Batch OCR] 在目录 {input_dir} 中未找到图片文件")
        return 1

    print(f"[Batch OCR] 找到 {len(files)} 张图片")
    print(f"[Batch OCR] 输出文件: {output_path}")

    with output_path.open("w", encoding="utf-8") as md_file:
        md_file.write("# OCR 识别结果汇总\n\n")
        md_file.write(f"- 源目录: `{input_dir}`\n")
        md_file.write(f"- 识别模式: `{args.prompt_style}`\n")
        md_file.write(f"- 模型: `{args.model}`\n\n")
        md_file.write("---\n\n")

        for index, image_path in enumerate(files, start=1):
            print(f"[{index}/{len(files)}] 正在识别: {image_path.name}")
            md_file.write(f"## 文件: {image_path.relative_to(input_dir)}\n\n")

            try:
                base64_image = encode_image_from_path(image_path)
                full_text = "".join(
                    generate_ocr_stream_local(
                        base64_image,
                        args.prompt_style,
                        model=args.model,
                        ollama_url=args.ollama_url,
                    )
                )
                md_file.write(full_text.strip())
                md_file.write("\n\n---\n\n")
                md_file.flush()
            except Exception as exc:
                error_msg = f"识别失败: {exc}"
                print(f"  [!] {error_msg}")
                md_file.write(f"> **错误**: {error_msg}\n\n---\n\n")

    print(f"[Batch OCR] 完成，结果已写入 {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
