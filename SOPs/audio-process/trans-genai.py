#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import json
import os
import re
import time
import uuid
from pathlib import Path

from dotenv import load_dotenv
from google import genai
from google.genai import types
from pydub import AudioSegment

load_dotenv()

SUPPORTED_EXTENSIONS = {".m4a", ".mp3", ".wav", ".flac", ".aac", ".ogg", ".mp4", ".mkv"}

PROMPT = """
请仔细听这段音频并将其转录为文字。

要求：
1. 逐字逐句转录，不要摘要。
2. 按时间轴分段，时间格式为 HH:MM:SS。
3. 忽略明显的背景噪音。
4. 如果多人说话，尽量区分 speaker。
5. 输出简体中文。
6. 直接输出结果，不要包含额外寒暄。
7. 每段包含 speaker、timestamp、content、emotion。
"""

TRANSCRIPT_SCHEMA = types.Schema(
    type=types.Type.OBJECT,
    properties={
        "summary": types.Schema(type=types.Type.STRING),
        "segments": types.Schema(
            type=types.Type.ARRAY,
            items=types.Schema(
                type=types.Type.OBJECT,
                properties={
                    "speaker": types.Schema(type=types.Type.STRING),
                    "timestamp": types.Schema(type=types.Type.STRING),
                    "content": types.Schema(type=types.Type.STRING),
                    "emotion": types.Schema(type=types.Type.STRING),
                },
                required=["timestamp", "content"],
            ),
        ),
    },
    required=["summary", "segments"],
)


def parse_args():
    parser = argparse.ArgumentParser(description="使用 Gemini 对音频目录执行批量转写。")
    parser.add_argument("input_dir", help="音频目录")
    parser.add_argument(
        "-o",
        "--output-dir",
        default="transcripts_genai",
        help="输出目录名或路径，默认在输入目录下创建 transcripts_genai",
    )
    parser.add_argument(
        "--chunk-minutes",
        type=int,
        default=10,
        help="音频切片长度（分钟），默认 10",
    )
    parser.add_argument(
        "--model",
        default="gemini-3-pro-preview",
        help="Gemini 模型名",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="即使输出已存在也重新生成",
    )
    return parser.parse_args()


def parse_timestamp_to_seconds(timestamp_str: str) -> int:
    if not timestamp_str:
        return 0

    match = re.search(r"(\d{1,2}):(\d{2}):(\d{2})", timestamp_str)
    if match:
        hours, minutes, seconds = map(int, match.groups())
        return hours * 3600 + minutes * 60 + seconds

    match = re.search(r"(\d{1,2}):(\d{2})", timestamp_str)
    if match:
        minutes, seconds = map(int, match.groups())
        return minutes * 60 + seconds

    return 0


def format_seconds_to_timestamp(seconds: float) -> str:
    total_seconds = int(seconds)
    hours = total_seconds // 3600
    minutes = (total_seconds % 3600) // 60
    secs = total_seconds % 60
    return f"{hours:02d}:{minutes:02d}:{secs:02d}"


def wait_for_files_active(client: genai.Client, file_name: str):
    print("⏳ 等待云端处理...", end="", flush=True)
    while True:
        file_obj = client.files.get(name=file_name)
        state = getattr(file_obj, "state", None)
        state_name = getattr(state, "name", state)
        if state_name == "ACTIVE":
            print(" 就绪！")
            return file_obj
        if state_name == "FAILED":
            raise RuntimeError("云端文件处理失败")
        time.sleep(2)


def save_transcript(data: dict, output_path: Path, raw_json_path: Path) -> None:
    raw_json_path.write_text(
        json.dumps(data, ensure_ascii=False, indent=2), encoding="utf-8"
    )

    with output_path.open("w", encoding="utf-8") as handle:
        handle.write(f"=== 摘要 ===\n{data.get('summary', '无')}\n\n")
        handle.write("=== 对话详情 ===\n")
        for seg in data.get("segments", []):
            timestamp = seg.get("timestamp", "00:00:00")
            speaker = seg.get("speaker", "未知说话人")
            emotion = seg.get("emotion", "未标注")
            content = seg.get("content", "")
            handle.write(f"[{timestamp}] {speaker} ({emotion}): {content}\n")


def process_audio_chunk(client: genai.Client, chunk_path: Path, model: str):
    uploaded_file = None
    try:
        print("    ⬆️ 上传分片...", end="", flush=True)
        uploaded_file = client.files.upload(file=str(chunk_path))
        wait_for_files_active(client, uploaded_file.name)

        print(" 🧠 分析中...", end="", flush=True)
        response = client.models.generate_content(
            model=model,
            contents=[
                types.Content(
                    parts=[
                        types.Part(
                            file_data=types.FileData(
                                file_uri=uploaded_file.uri,
                                mime_type=uploaded_file.mime_type,
                            )
                        ),
                        types.Part(text=PROMPT),
                    ]
                )
            ],
            config=types.GenerateContentConfig(
                response_mime_type="application/json",
                response_schema=TRANSCRIPT_SCHEMA,
            ),
        )

        if not response.text:
            raise RuntimeError("API 返回空内容")
        return json.loads(response.text)
    finally:
        if uploaded_file:
            try:
                client.files.delete(name=uploaded_file.name)
            except Exception:
                pass


def main() -> int:
    args = parse_args()
    api_key = os.getenv("GEMINI_API_KEY")
    if not api_key:
        raise EnvironmentError("未找到 GEMINI_API_KEY，请在 .env 或环境变量中设置。")

    input_dir = Path(args.input_dir).expanduser().resolve()
    if not input_dir.is_dir():
        print(f"❌ 输入目录不存在: {input_dir}")
        return 1

    output_dir = Path(args.output_dir).expanduser()
    if not output_dir.is_absolute():
        output_dir = input_dir / output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    files = sorted(
        path for path in input_dir.iterdir() if path.suffix.lower() in SUPPORTED_EXTENSIONS
    )
    if not files:
        print(f"❌ 在 {input_dir} 中未找到音频文件。")
        return 1

    client = genai.Client(api_key=api_key)
    print(f"🚀 任务列表: {len(files)} 个文件")

    failures = 0
    for index, audio_path in enumerate(files, start=1):
        output_txt = output_dir / f"{audio_path.stem}.txt"
        output_json = output_dir / f"{audio_path.stem}.json"

        if output_txt.exists() and not args.force:
            print(f"⏭️ 跳过已存在: {audio_path.name}")
            continue

        print(f"\n[{index}/{len(files)}] 📂 处理: {audio_path.name}")

        try:
            print("  🔄 加载音频中...", end="", flush=True)
            audio = AudioSegment.from_file(audio_path)
            duration_ms = len(audio)
            print(f" 长度: {duration_ms / 1000 / 60:.2f} 分钟")
        except Exception as exc:
            failures += 1
            print(f"\n❌ 读取音频失败: {exc}")
            print("提示: 请确保已安装 ffmpeg。")
            continue

        chunk_length_ms = args.chunk_minutes * 60 * 1000
        total_chunks = max(1, (duration_ms + chunk_length_ms - 1) // chunk_length_ms)
        all_segments = []
        all_summaries = []

        for chunk_index in range(total_chunks):
            start_ms = chunk_index * chunk_length_ms
            end_ms = min((chunk_index + 1) * chunk_length_ms, duration_ms)
            chunk = audio[start_ms:end_ms]
            chunk_path = input_dir / f"temp_{uuid.uuid4().hex}_chunk_{chunk_index}.mp3"

            print(
                f"  🔪 分片 [{chunk_index + 1}/{total_chunks}] ({start_ms // 1000}s - {end_ms // 1000}s)...",
                end="",
                flush=True,
            )

            try:
                chunk.export(chunk_path, format="mp3", bitrate="128k")
                result = process_audio_chunk(client, chunk_path, args.model)
                print(" ✅")

                if result.get("summary"):
                    all_summaries.append(f"[分片{chunk_index + 1}] {result['summary']}")

                for seg in result.get("segments", []):
                    seconds = parse_timestamp_to_seconds(seg.get("timestamp", ""))
                    seg["timestamp"] = format_seconds_to_timestamp(seconds + start_ms / 1000)
                    all_segments.append(seg)
            except Exception as exc:
                print(f" ❌ 分片处理失败: {exc}")
            finally:
                chunk_path.unlink(missing_ok=True)

        final_result = {"summary": "\n".join(all_summaries), "segments": all_segments}
        save_transcript(final_result, output_txt, output_json)
        print(f"🎉 文件 {audio_path.name} 处理完成！")
        time.sleep(1)

    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
