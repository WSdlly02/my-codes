# -*- coding: utf-8 -*-
import os
import time
import argparse
from dotenv import load_dotenv
from pathlib import Path
from google import genai

load_dotenv()  # 从 .env 文件加载环境变量
# 配置区域
API_KEY = os.getenv("GEMINI_API_KEY")
if API_KEY is None:
    raise EnvironmentError("未找到 GEMINI_API_KEY，请在 .env 文件中设置。")
client = genai.Client(api_key=API_KEY)


def process_pdfs(dir_path):
    SOURCE_FOLDER = dir_path  # PDF 存放目录
    OUTPUT_FOLDER = Path(SOURCE_FOLDER) / "genai_ocr_outputs"  # 输出目录

    output_path = Path(OUTPUT_FOLDER)
    output_path.mkdir(exist_ok=True)

    # 获取所有 PDF
    pdf_files = list(Path(SOURCE_FOLDER).glob("*.pdf"))

    if not pdf_files:
        print(f"在 {SOURCE_FOLDER} 中未找到 PDF 文件。")
        return

    print(f"找到 {len(pdf_files)} 个 PDF 文件。")

    for pdf_file in pdf_files:
        try:
            # 使用 .name 打印日志（Python 终端通常支持中文显示）
            print(f"\n--- 正在处理: {pdf_file.name} ---")

            # --- 关键修改点 1: 以二进制流读取并重命名 display_name ---
            # 我们给 Gemini 传递一个临时的 display_name，避免 header 编码错误
            with open(pdf_file, "rb") as f:
                uploaded_file = client.files.upload(
                    file=f,
                    config=genai.types.UploadFileConfig(
                        display_name="temp_pdf_upload",  # 使用 ASCII 字符作为显示名
                        mime_type="application/pdf",
                    ),
                )

            prompt = """
            任务：将此 PDF 提取为 Markdown 格式。
            
            要求：
            1. 每一页前插入 "## Page [页码]"。
            2. 去除页眉、页脚和页码。
            3. 识别表格(Markdown Table)和公式(LaTeX)。
            4. 直接返回结果，不要解释。
            """

            # 调用模型
            response = client.models.generate_content(
                model="gemini-3-flash-preview", contents=[uploaded_file, prompt]
            )

            # --- 关键修改点 2: 确保写入文件时使用 utf-8 编码 ---
            output_filename = output_path / f"{pdf_file.stem}.md"
            with open(output_filename, "w", encoding="utf-8") as f_out:
                f_out.write(response.text)

            print(f"提取成功: {output_filename.name}")

            # 这里的清理建议保留，避免云端文件堆积
            client.files.delete(name=uploaded_file.name)

            # 免费版频率控制
            time.sleep(3)

        except Exception as e:
            print(f"处理 {pdf_file.name} 时出错: {str(e)}")


def main():
    parser = argparse.ArgumentParser(
        description="批量将文件夹下的 PDF 使用 Gemini API 进行 OCR 识别汇总"
    )
    parser.add_argument("dir_path", help="包含 PDF 文件的文件夹路径")
    args = parser.parse_args()
    process_pdfs(args.dir_path)


if __name__ == "__main__":
    main()
