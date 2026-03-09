#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import subprocess
from pathlib import Path

SUPPORTED_EXTENSIONS = {".mp3", ".wav", ".m4a", ".mp4", ".flac", ".mkv", ".aac", ".ogg"}


def post_process_srt(srt_path: Path, output_path: Path) -> None:
    if not srt_path.exists():
        return

    lines = srt_path.read_text(encoding="utf-8").splitlines()
    processed_lines = []
    index = 0

    while index < len(lines):
        line = lines[index].strip()
        if not line.isdigit():
            index += 1
            continue

        index += 1
        if index >= len(lines):
            break

        time_line = lines[index].strip()
        if " --> " not in time_line:
            index += 1
            continue

        start, end = time_line.split(" --> ", maxsplit=1)

        def format_time(value: str) -> str:
            value = value.replace(",", ".")
            return value[3:] if value.startswith("00:") else value

        new_time = f"[{format_time(start)} --> {format_time(end)}]"
        index += 1
        text_parts = []
        while index < len(lines) and lines[index].strip():
            text_parts.append(lines[index].strip())
            index += 1
        processed_lines.append(f"{new_time} {' '.join(text_parts)}")

    output_path.write_text("\n".join(processed_lines), encoding="utf-8")
    srt_path.unlink(missing_ok=True)


def parse_args():
    parser = argparse.ArgumentParser(
        description="使用本地 Whisper 批量转写音频文件，并输出整理后的 txt。"
    )
    parser.add_argument("input_dir", help="音频目录")
    parser.add_argument(
        "-o",
        "--output-dir",
        default="transcripts_local",
        help="输出目录名或路径，默认在输入目录下创建 transcripts_local",
    )
    parser.add_argument("--model", default="large-v3-turbo", help="Whisper 模型名")
    parser.add_argument("--language", default="Chinese", help="Whisper 语言参数")
    parser.add_argument(
        "--output-format",
        default="srt",
        choices=["srt", "txt", "vtt", "tsv", "json"],
        help="whisper.sh 输出格式，默认 srt",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="即使输出已存在也重新处理",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    input_dir = Path(args.input_dir).expanduser().resolve()
    if not input_dir.is_dir():
        print(f"❌ 错误: 目录不存在 {input_dir}")
        return 1

    output_dir = Path(args.output_dir).expanduser()
    if not output_dir.is_absolute():
        output_dir = input_dir / output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    script_dir = Path(__file__).resolve().parent
    whisper_script = script_dir / "whisper.sh"
    if not whisper_script.is_file():
        print(f"❌ 错误: 未找到 {whisper_script}")
        return 1

    files = sorted(
        path for path in input_dir.iterdir() if path.suffix.lower() in SUPPORTED_EXTENSIONS
    )
    if not files:
        print(f"📂 {input_dir} 中没有找到支持的音频文件。")
        return 1

    print(f"📋 任务列表: 共 {len(files)} 个文件")
    print("=" * 60)

    failures = 0
    for index, audio_path in enumerate(files, start=1):
        file_stem = audio_path.stem
        final_txt_path = output_dir / f"{file_stem}.txt"
        raw_output_path = output_dir / f"{file_stem}.{args.output_format}"

        if final_txt_path.exists() and not args.force:
            print(f"[{index}/{len(files)}] ⏭️ 跳过 (已存在): {audio_path.name}")
            continue

        print(f"\n[{index}/{len(files)}] 🎙️ 正在处理: {audio_path.name}")
        print("-" * 30)

        try:
            cmd = [
                "bash",
                str(whisper_script),
                audio_path.name,
                "--model",
                args.model,
                "--language",
                args.language,
                "--output_dir",
                os.path.relpath(output_dir, input_dir),
                "--output_format",
                args.output_format,
            ]
            subprocess.run(cmd, check=True, cwd=input_dir)

            if args.output_format == "srt":
                post_process_srt(raw_output_path, final_txt_path)
            elif raw_output_path.exists():
                if raw_output_path != final_txt_path:
                    final_txt_path.write_text(
                        raw_output_path.read_text(encoding="utf-8"), encoding="utf-8"
                    )
                else:
                    final_txt_path = raw_output_path

            print(f"✅ 处理完成: {audio_path.name} -> {final_txt_path.name}")
        except subprocess.CalledProcessError as exc:
            failures += 1
            print(f"❌ 处理失败 {audio_path.name}: {exc}")
        except Exception as exc:
            failures += 1
            print(f"❌ 未知错误 {audio_path.name}: {exc}")

    print("\n🎉 所有文件处理完毕。")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
