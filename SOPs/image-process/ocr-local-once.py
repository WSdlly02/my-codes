#! /usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import argparse
import subprocess
import base64
from urllib.parse import unquote
from ocrcore import generate_ocr_stream_local, DEFAULT_PROMPT_MAP


def encode_image_from_path(image_path) -> str:
    """将图片文件转换为 base64 编码"""
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


def get_image_from_clipboard():
    """从剪贴板获取图片数据或文件引用"""
    # 1. 尝试获取文件引用 (如 file:///path/to/image.png)
    try:
        result = subprocess.run(
            ["wl-paste", "--no-newline", "--type", "text/uri-list"],
            capture_output=True,
            text=True,
        )
        content = result.stdout.strip()
        if content.startswith("file://"):
            path = unquote(content[7:])
            print(f"[OCR] 检测到剪贴板中的文件引用: {path}")
            return encode_image_from_path(path), f"文件引用: {path}"
        elif content.startswith("/") and (
            content.endswith(".png")
            or content.endswith(".jpg")
            or content.endswith(".jpeg")
        ):
            print(f"[OCR] 检测到剪贴板中的路径字符串: {content}")
            return encode_image_from_path(content), f"路径引用: {content}"
    except Exception:
        pass

    # 2. 尝试直接获取图片二进制数据
    try:
        result = subprocess.run(
            ["wl-paste", "--no-newline", "--type", "image/png"],
            capture_output=True,
            check=True,
        )
        if result.stdout:
            return base64.b64encode(result.stdout).decode("utf-8"), "剪贴板图片数据"
    except subprocess.CalledProcessError:
        pass
    except FileNotFoundError:
        print("[OCR] 错误：未找到 'wl-paste' 命令。请确保已安装 wl-clipboard。")

    return None, None


def main():
    parser = argparse.ArgumentParser(description="使用 qwen3-vl:8b 进行 OCR 识别的脚本")
    parser.add_argument(
        "image_path", nargs="?", help="图片文件的路径（可选，默认从剪贴板获取）"
    )
    parser.add_argument(
        "-p",
        "--prompt-style",
        choices=list(DEFAULT_PROMPT_MAP.keys()),
        default="t",
        help="识别风格: t (纯文本), md (Markdown), f (数学公式/LaTeX), table (表格), json (JSON格式), desc (详细描述图片)",
    )
    args = parser.parse_args()

    base64_image = None
    source_info = ""
    headless = os.getenv("DISPLAY") is None
    if headless:
        print("[OCR] 处于无头模式，跳过剪贴板操作。")

    # 检查命令行参数
    if args.image_path:
        try:
            base64_image = encode_image_from_path(args.image_path)
            source_info = f"[OCR] 文件: {args.image_path}"
        except FileNotFoundError:
            print(f"[OCR] 错误：找不到文件 {args.image_path}")
            return
    elif not headless:
        # 尝试从剪贴板获取
        print("[OCR] 未指定路径，尝试从剪贴板获取图片...")
        base64_image, source_info = get_image_from_clipboard()
    else:
        print("[OCR] 无头模式且未指定图片路径，无法获取图片，退出。")
        return

    if not base64_image:
        print("[OCR] 无法获取图片数据（剪贴板为空或格式不支持），退出。")
        return

    print(
        f"[OCR] 正在使用 qwen3-vl:8b (模式:{args.prompt_style}) 识别 {source_info}..."
    )

    full_text = ""
    try:
        for content in generate_ocr_stream_local(base64_image, args.prompt_style):
            print(content, end="", flush=True)
            full_text += content

        # 自动复制到剪贴板
        if full_text.strip() and not headless:
            subprocess.run(["wl-copy"], input=full_text.encode("utf-8"), check=True)
            print("\n\n[OCR] 结果已自动复制到剪贴板。")
    except Exception as e:
        print(f"\n[OCR] 请求出错: {e}")

    print("\n[OCR] 识别完成。")


if __name__ == "__main__":
    main()
