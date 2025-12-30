#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import subprocess
from pathlib import Path

# ================= 配置区域 =================
INPUT_FOLDER = "/home/wsdlly02/Disks/Files/Files/College/录音整理/audio_files"
OUTPUT_FOLDER_RELATIVE = "transcripts_local"
OUTPUT_FOLDER = Path(INPUT_FOLDER) / OUTPUT_FOLDER_RELATIVE
SUPPORTED_EXTENSIONS = {".mp3", ".wav", ".m4a", ".mp4", ".flac", ".mkv"}
MODEL = "large-v3-turbo"
LANGUAGE = "Chinese"
# ===========================================


def post_process_srt(srt_path, output_path):
    """将 SRT 格式转换为自定义的时间戳格式 [MM:SS.mmm --> MM:SS.mmm] Text"""
    if not os.path.exists(srt_path):
        return

    with open(srt_path, "r", encoding="utf-8") as f:
        lines = f.readlines()

    processed_lines = []
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line.isdigit():  # 序号行
            i += 1
            if i < len(lines):
                time_line = lines[i].strip()
                if " --> " in time_line:
                    # 格式: 00:10:32,860 --> 00:10:35,300
                    start, end = time_line.split(" --> ")

                    def format_time(t):
                        t = t.replace(",", ".")
                        # 如果是 00: 开头则去掉，保留 MM:SS.mmm
                        if t.startswith("00:"):
                            return t[3:]
                        return t

                    new_time = f"[{format_time(start)} --> {format_time(end)}]"
                    i += 1
                    text_parts = []
                    # 读取接下来的文本行，直到遇到空行
                    while i < len(lines) and lines[i].strip() != "":
                        text_parts.append(lines[i].strip())
                        i += 1
                    text = " ".join(text_parts)
                    processed_lines.append(f"{new_time} {text}")
        i += 1

    with open(output_path, "w", encoding="utf-8") as f:
        f.write("\n".join(processed_lines))

    # 处理完后删除原始 srt 文件
    os.remove(srt_path)


def batch_transcribe():
    # 1. 准备目录
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)

    if not os.path.exists(INPUT_FOLDER):
        print(f"❌ 错误: 文件夹 '{INPUT_FOLDER}' 不存在。")
        return

    # 2. 扫描文件
    files = [
        f
        for f in os.listdir(INPUT_FOLDER)
        if os.path.splitext(f)[1].lower() in SUPPORTED_EXTENSIONS
    ]
    total_files = len(files)

    if total_files == 0:
        print(f"📂 '{INPUT_FOLDER}' 中没有找到支持的音频文件。")
        return

    # 获取 whisper.sh 的绝对路径
    script_dir = os.path.dirname(os.path.abspath(__file__))
    whisper_script = os.path.join(script_dir, "whisper.sh")

    print(f"📋 任务列表: 共 {total_files} 个文件")
    print("=" * 60)

    # 3. 开始循环处理
    for index, filename in enumerate(files):
        file_stem = Path(filename).stem
        output_path = OUTPUT_FOLDER / f"{file_stem}.txt"
        srt_file = OUTPUT_FOLDER / f"{file_stem}.srt"

        # 如果输出文件已存在，则跳过
        if output_path.exists():
            print(f"[{index+1}/{total_files}] ⏭️ 跳过 (已存在): {filename}")
            continue

        # 如果 SRT 文件已存在，则直接进行后处理
        if srt_file.exists():
            print(
                f"[{index+1}/{total_files}] ⏭️ 字幕文件已存在，直接进行后处理: {filename}"
            )
            post_process_srt(srt_file, output_path)
            continue

        # Whisper 默认会生成多种格式，我们主要关注 .srt 或 .txt
        # 这里我们让 whisper.sh 处理，并指定输出目录
        print(f"\n[{index+1}/{total_files}] 🎙️ 正在处理: {filename}")
        print("-" * 30)

        try:
            # 调用 whisper.sh
            # 参数说明:
            # --model large-v3: 使用大模型
            # --language Chinese: 指定语言
            # --output_dir: 指定输出目录
            # --output_format srt: 指定输出格式
            cmd = [
                "bash",
                whisper_script,
                filename,
                "--model",
                MODEL,
                "--language",
                LANGUAGE,
                "--output_dir",
                OUTPUT_FOLDER_RELATIVE,
                "--output_format",
                "srt",
            ]

            # 使用 subprocess.run 并实时打印输出
            # 设置 cwd 为 INPUT_FOLDER，这样 whisper.sh 挂载的就是音频所在目录
            subprocess.run(cmd, check=True, cwd=INPUT_FOLDER)

            # 后处理：将生成的 .srt 转换为自定义格式的 .txt

            post_process_srt(srt_file, output_path)

            print("-" * 30)
            print(f"✅ 处理完成并已转换格式: {filename}")

        except subprocess.CalledProcessError as e:
            print(f"\n❌ 处理出错 {filename}: {str(e)}")
        except Exception as e:
            print(f"\n❌ 发生未知错误 {filename}: {str(e)}")

    print("\n🎉 所有文件处理完毕！")


if __name__ == "__main__":
    batch_transcribe()
