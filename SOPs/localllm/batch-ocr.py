#! /usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import argparse
import base64
from core import generate_ocr_stream, DEFAULT_PROMPT_MAP


def encode_image_from_path(image_path) -> str:
    """将图片文件转换为 base64 编码"""
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


def is_image_file(filename):
    """检查文件是否为支持的图片格式"""
    extensions = (".png", ".jpg", ".jpeg", ".webp")
    return filename.lower().endswith(extensions)


def main():
    parser = argparse.ArgumentParser(
        description="批量对文件夹下的图片进行 OCR 识别并汇总到 Markdown"
    )
    parser.add_argument("dir_path", help="包含图片的文件夹路径")
    parser.add_argument(
        "-o",
        "--output",
        default="ocr_results.md",
        help="输出的 Markdown 文件名 (默认: ocr_results.md)",
    )
    parser.add_argument(
        "-p",
        "--prompt-style",
        choices=list(DEFAULT_PROMPT_MAP.keys()),
        default="t",
        help="识别风格: t (纯文本), md (Markdown), f (数学公式/LaTeX), table (表格), json (JSON格式), desc (详细描述图片)",
    )
    args = parser.parse_args()

    if not os.path.isdir(args.dir_path):
        print(f"[Batch OCR] 错误：{args.dir_path} 不是一个有效的目录")
        return

    # 获取所有图片文件并排序
    files = [f for f in os.listdir(args.dir_path) if is_image_file(f)]
    files.sort()

    if not files:
        print(f"[Batch OCR] 在目录 {args.dir_path} 中未找到图片文件")
        return

    output_path = os.path.join(args.dir_path, args.output)
    print(f"[Batch OCR] 找到 {len(files)} 张图片，准备开始识别...")
    print(f"[Batch OCR] 结果将保存至: {output_path}")

    with open(output_path, "w", encoding="utf-8") as md_file:
        md_file.write(f"# OCR 识别结果汇总\n\n")
        md_file.write(f"- **源目录**: `{args.dir_path}`\n")
        md_file.write(f"- **识别模式**: `{args.prompt_style}`\n\n")
        md_file.write("---\n\n")

        for i, filename in enumerate(files, 1):
            full_path = os.path.join(args.dir_path, filename)
            print(f"[{i}/{len(files)}] 正在识别: {filename}...")

            md_file.write(f"## 文件: {filename}\n\n")

            try:
                base64_image = encode_image_from_path(full_path)

                full_text = ""
                for content in generate_ocr_stream(
                    base64_image,
                    args.prompt_style,
                    model="qwen3-vl:4b",  # 使用更小的模型
                ):
                    full_text += content

                md_file.write(full_text.strip())
                md_file.write("\n\n---\n\n")
                md_file.flush()  # 实时写入
            except Exception as e:
                error_msg = f"识别失败: {e}"
                print(f"  [!] {error_msg}")
                md_file.write(f"> **错误**: {error_msg}\n\n---\n\n")

    print(f"\n[Batch OCR] 全部处理完成！结果已保存至 {output_path}")


if __name__ == "__main__":
    main()
