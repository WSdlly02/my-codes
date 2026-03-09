# -*- coding: utf-8 -*-

import argparse
import os
import time
from pathlib import Path

from dotenv import load_dotenv
from google import genai

load_dotenv()

DEFAULT_PROMPT = """
任务：将此 PDF 提取为 Markdown 格式。

要求：
1. 每一页前插入 "## Page [页码]"。
2. 去除页眉、页脚和页码。
3. 识别表格为 Markdown Table，识别公式为 LaTeX。
4. 直接返回结果，不要解释。
"""


def process_pdfs(dir_path: Path, output_dir: Path, model: str, force: bool) -> int:
    api_key = os.getenv("GEMINI_API_KEY")
    if not api_key:
        raise EnvironmentError("未找到 GEMINI_API_KEY，请在 .env 或环境变量中设置。")

    client = genai.Client(api_key=api_key)
    output_dir.mkdir(parents=True, exist_ok=True)

    pdf_files = sorted(dir_path.glob("*.pdf"))
    if not pdf_files:
        print(f"在 {dir_path} 中未找到 PDF 文件。")
        return 1

    print(f"找到 {len(pdf_files)} 个 PDF 文件。")
    failures = 0

    for pdf_file in pdf_files:
        output_file = output_dir / f"{pdf_file.stem}.md"
        if output_file.exists() and not force:
            print(f"跳过已存在: {output_file.name}")
            continue

        uploaded_file = None
        try:
            print(f"\n--- 正在处理: {pdf_file.name} ---")
            with pdf_file.open("rb") as handle:
                uploaded_file = client.files.upload(
                    file=handle,
                    config=genai.types.UploadFileConfig(
                        display_name="temp_pdf_upload",
                        mime_type="application/pdf",
                    ),
                )

            response = client.models.generate_content(
                model=model,
                contents=[uploaded_file, DEFAULT_PROMPT],
            )

            output_file.write_text(response.text or "", encoding="utf-8")
            print(f"提取成功: {output_file.name}")
            time.sleep(3)
        except Exception as exc:
            failures += 1
            print(f"处理 {pdf_file.name} 时出错: {exc}")
        finally:
            if uploaded_file:
                try:
                    client.files.delete(name=uploaded_file.name)
                except Exception:
                    pass

    return 1 if failures else 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="批量将目录中的 PDF 使用 Gemini 提取为 Markdown。"
    )
    parser.add_argument("dir_path", help="PDF 文件目录")
    parser.add_argument(
        "-o",
        "--output-dir",
        default="genai_ocr_outputs",
        help="输出目录名或路径，默认在输入目录下创建 genai_ocr_outputs",
    )
    parser.add_argument(
        "--model",
        default="gemini-3-flash-preview",
        help="Gemini 模型名",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="即使输出已存在也重新生成",
    )
    args = parser.parse_args()

    input_dir = Path(args.dir_path).expanduser().resolve()
    if not input_dir.is_dir():
        print(f"输入目录不存在: {input_dir}")
        return 1

    output_dir = Path(args.output_dir).expanduser()
    if not output_dir.is_absolute():
        output_dir = input_dir / output_dir

    return process_pdfs(input_dir, output_dir, args.model, args.force)


if __name__ == "__main__":
    raise SystemExit(main())
