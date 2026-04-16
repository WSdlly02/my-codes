#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import re
import time
from datetime import datetime
from pathlib import Path
from shutil import rmtree

import requests
from PIL import Image
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

DEFAULT_HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Accept": "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8",
    "Accept-Language": "zh-CN,zh;q=0.9,en;q=0.8",
    "Accept-Encoding": "gzip, deflate, br",
    "Referer": "https://cldisk.com/",
    "Connection": "keep-alive",
    "Sec-Fetch-Dest": "image",
    "Sec-Fetch-Mode": "no-cors",
    "Sec-Fetch-Site": "cross-site",
}

SUPPORTED_IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".tif", ".tiff"}


def build_session(headers: dict, retries: int) -> requests.Session:
    session = requests.Session()
    retry = Retry(
        total=retries, backoff_factor=1, status_forcelist=[500, 502, 503, 504]
    )
    adapter = HTTPAdapter(max_retries=retry)
    session.mount("http://", adapter)
    session.mount("https://", adapter)
    session.headers.update(headers)
    return session


def normalize_base_url(base_url: str) -> str:
    return base_url if base_url.endswith("/") else f"{base_url}/"


def ensure_resume_consistency(folder: Path, base_url: str) -> None:
    base_url_file = folder / "base_url.txt"
    normalized_base_url = normalize_base_url(base_url)

    if base_url_file.exists():
        saved_url = base_url_file.read_text(encoding="utf-8").strip()
        if saved_url != normalized_base_url:
            print("检测到 base_url 不一致，清除旧数据并重新开始...")
            rmtree(folder, ignore_errors=True)
    elif folder.exists():
        print("检测到下载目录存在但缺少标识文件，清除并重新开始...")
        rmtree(folder, ignore_errors=True)

    folder.mkdir(parents=True, exist_ok=True)
    base_url_file.write_text(normalized_base_url, encoding="utf-8")


def download_images(
    session: requests.Session,
    base_url: str,
    folder: Path,
    start_page: int,
    extension: str,
    min_bytes: int,
    max_failures: int,
) -> Path:
    ensure_resume_consistency(folder, base_url)
    page = start_page
    consecutive_failures = 0

    print("开始下载图片...")
    while consecutive_failures < max_failures:
        file_path = folder / f"{page}.{extension}"
        if file_path.exists():
            print(f"已存在，跳过: {file_path.name}")
            page += 1
            consecutive_failures = 0
            continue

        url = f"{base_url}{page}.{extension}"
        try:
            response = session.get(url, timeout=30)
            if response.status_code == 200 and len(response.content) >= min_bytes:
                file_path.write_bytes(response.content)
                print(f"下载成功: {file_path.name}")
                page += 1
                consecutive_failures = 0
                time.sleep(0.2)
                continue

            print(f"第 {page} 页无内容或错误 (状态码 {response.status_code})")
        except Exception as exc:
            print(f"第 {page} 页下载出错: {exc}")

        consecutive_failures += 1
        if consecutive_failures < max_failures:
            time.sleep(2)

    print(f"下载结束：连续失败 {consecutive_failures} 次，第 {page} 页可能不存在")
    return folder


def image_sort_key(image_path: Path) -> tuple[int, str]:
    match = re.search(r"(\d+)$", image_path.stem)
    if match:
        return int(match.group(1)), image_path.name
    return 10**18, image_path.name


def collect_images_from_folder(folder: Path) -> list[Path]:
    if not folder.exists():
        print(f"图片目录不存在: {folder}")
        return []
    if not folder.is_dir():
        print(f"指定的图片路径不是目录: {folder}")
        return []

    image_paths = [
        path
        for path in folder.iterdir()
        if path.is_file() and path.suffix.lower() in SUPPORTED_IMAGE_EXTENSIONS
    ]
    image_paths.sort(key=image_sort_key)

    if not image_paths:
        print(f"目录中没有可合并的图片: {folder}")
        return []

    print(f"从目录读取到 {len(image_paths)} 张图片: {folder}")
    return image_paths


def merge_images_to_pdf(image_paths: list[Path], output_pdf: Path) -> int:
    if not image_paths:
        print("没有可合并的图片，退出。")
        return 1

    valid_images = []
    for image_path in image_paths:
        try:
            with Image.open(image_path) as img:
                valid_images.append(img.convert("RGB"))
        except Exception as exc:
            print(f"打开图片失败 {image_path}: {exc}")

    if not valid_images:
        print("没有有效的图片可合并。")
        return 1

    valid_images[0].save(output_pdf, save_all=True, append_images=valid_images[1:])
    print(f"合并完成！PDF 已保存为: {output_pdf}")
    print(f"共 {len(valid_images)} 页")
    return 0


def resolve_image_paths(args: argparse.Namespace) -> list[Path]:
    if args.image_dir:
        image_dir = Path(args.image_dir).expanduser().resolve()
        return collect_images_from_folder(image_dir)

    session = build_session(DEFAULT_HEADERS, retries=3)
    download_folder = download_images(
        session=session,
        base_url=normalize_base_url(args.base_url),
        folder=Path(args.folder).expanduser().resolve(),
        start_page=args.start_page,
        extension=args.extension.lstrip("."),
        min_bytes=args.min_bytes,
        max_failures=args.max_failures,
    )
    return collect_images_from_folder(download_folder)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="按页码下载图片或读取本地图片目录并合并成 PDF。"
    )
    parser.add_argument(
        "base_url",
        nargs="?",
        help="分页图片的基础 URL，以页码和扩展名结尾前的部分为准",
    )
    parser.add_argument(
        "--image-dir",
        help="本地图片目录，目录内图片将按文件名中的数字顺序合并",
    )
    parser.add_argument(
        "-s",
        "--start-page",
        type=int,
        default=1,
        help="起始页码，默认 1",
    )
    parser.add_argument(
        "-f",
        "--folder",
        default="downloaded_images",
        help="图片下载目录，默认 downloaded_images",
    )
    parser.add_argument(
        "-o",
        "--output-pdf",
        default=f"{datetime.now().strftime('%Y%m%d_%H%M%S')}.pdf",
        help="输出 PDF 路径",
    )
    parser.add_argument(
        "--extension",
        default="png",
        help="图片扩展名，默认 png",
    )
    parser.add_argument(
        "--min-bytes",
        type=int,
        default=1000,
        help="判定有效图片的最小字节数，默认 1000",
    )
    parser.add_argument(
        "--max-failures",
        type=int,
        default=1,
        help="连续失败多少次后停止，默认 1",
    )
    args = parser.parse_args()

    output_pdf = Path(args.output_pdf).expanduser().resolve()
    output_pdf.parent.mkdir(parents=True, exist_ok=True)

    if not args.image_dir and not args.base_url:
        parser.error("必须提供 base_url，或使用 --image-dir 指定本地图片目录")

    image_paths = resolve_image_paths(args)
    return merge_images_to_pdf(image_paths, output_pdf)


if __name__ == "__main__":
    raise SystemExit(main())
