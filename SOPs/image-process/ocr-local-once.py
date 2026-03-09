#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import base64
import os
import subprocess
from pathlib import Path
from urllib.parse import unquote

from ocrcore import DEFAULT_MODEL, DEFAULT_PROMPT_MAP, generate_ocr_stream_local


def encode_image_from_path(image_path: Path) -> str:
    with image_path.open("rb") as image_file:
        return base64.b64encode(image_file.read()).decode("utf-8")


def get_image_from_clipboard():
    try:
        result = subprocess.run(
            ["wl-paste", "--no-newline", "--type", "text/uri-list"],
            capture_output=True,
            text=True,
            check=False,
        )
        content = result.stdout.strip()
        if content.startswith("file://"):
            path = Path(unquote(content[7:])).expanduser().resolve()
            print(f"[OCR] 检测到剪贴板文件引用: {path}")
            return encode_image_from_path(path), f"文件引用: {path}"
        if content.startswith("/"):
            path = Path(content).expanduser().resolve()
            if path.exists():
                print(f"[OCR] 检测到剪贴板路径: {path}")
                return encode_image_from_path(path), f"路径引用: {path}"
    except Exception:
        pass

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
        print("[OCR] 错误：未找到 `wl-paste`，请安装 wl-clipboard。")

    return None, None


def main() -> int:
    parser = argparse.ArgumentParser(description="对单张图片执行本地 OCR。")
    parser.add_argument("image_path", nargs="?", help="图片路径；留空时尝试从剪贴板读取")
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
        "--ollama-url",
        default="http://localhost:11434/api/generate",
        help="Ollama generate API 地址",
    )
    parser.add_argument(
        "--no-copy",
        action="store_true",
        help="识别完成后不自动复制结果到剪贴板",
    )
    args = parser.parse_args()

    base64_image = None
    source_info = ""
    headless = os.getenv("WAYLAND_DISPLAY") is None and os.getenv("DISPLAY") is None

    if args.image_path:
        image_path = Path(args.image_path).expanduser().resolve()
        if not image_path.is_file():
            print(f"[OCR] 错误：找不到文件 {image_path}")
            return 1
        base64_image = encode_image_from_path(image_path)
        source_info = f"文件: {image_path}"
    elif not headless:
        print("[OCR] 未指定路径，尝试从剪贴板获取图片...")
        base64_image, source_info = get_image_from_clipboard()
    else:
        print("[OCR] 当前为无图形环境，且未提供图片路径。")
        return 1

    if not base64_image:
        print("[OCR] 无法获取图片数据。")
        return 1

    print(f"[OCR] 开始识别 {source_info}，模型: {args.model}，风格: {args.prompt_style}")

    full_text = ""
    try:
        for content in generate_ocr_stream_local(
            base64_image,
            args.prompt_style,
            model=args.model,
            ollama_url=args.ollama_url,
        ):
            print(content, end="", flush=True)
            full_text += content
    except Exception as exc:
        print(f"\n[OCR] 请求出错: {exc}")
        return 1

    if full_text.strip() and not headless and not args.no_copy:
        try:
            subprocess.run(["wl-copy"], input=full_text.encode("utf-8"), check=True)
            print("\n\n[OCR] 结果已复制到剪贴板。")
        except FileNotFoundError:
            print("\n\n[OCR] 未找到 `wl-copy`，跳过复制。")

    print("\n[OCR] 识别完成。")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
