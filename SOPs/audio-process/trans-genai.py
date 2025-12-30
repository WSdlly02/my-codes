import os
import time
import json
import uuid
import re
from dotenv import load_dotenv
from pathlib import Path
from google import genai
from google.genai import types
from pydub import AudioSegment

load_dotenv()  # 从 .env 文件加载环境变量
# ================= 配置区域 =================
API_KEY = os.getenv("GEMINI_API_KEY")
if API_KEY is None:
    raise EnvironmentError("未找到 GEMINI_API_KEY，请在 .env 文件中设置。")
MODEL_NAME = "gemini-3-pro-preview"
INPUT_FOLDER = "/home/wsdlly02/Disks/Files/Files/College/录音整理/audio_files"
OUTPUT_FOLDER = Path(INPUT_FOLDER) / "transcripts_genai"
CHUNK_MINUTES = 10  # 每个分片 10 分钟
# ===========================================

client = genai.Client(api_key=API_KEY)

PROMPT = """
请仔细听这段音频并将其转录为文字。
这是一个非常重要的任务，请务必完整转录，不要遗漏任何细节。

要求：
1. 【重要】请逐字逐句转录，不要进行摘要或概括。
2. 请按时间轴分段，格式为：[MM:SS] 文字内容。
3. 请忽略背景中的电流声、风声或其他非人声噪音。
4. 如果有多人说话，请尝试区分（例如：[00:10] 说话人A: ...）。
5. 输出简体中文。
6. 直接输出结果，不要包含"好的"、"这是转录结果"等废话。
7. 将对话分段，每段包含说话人、时间戳、内容和情感标签（如：高兴、悲伤、愤怒、平静等）。
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
            ),
        ),
    },
    required=["summary", "segments"],
)


def parse_timestamp_to_seconds(timestamp_str):
    """将 [MM:SS] 或 [HH:MM:SS] 转换为秒数"""
    if not timestamp_str:
        return 0

    # 优先匹配 HH:MM:SS
    match = re.search(r"(\d{1,2}):(\d{2}):(\d{2})", timestamp_str)
    if match:
        h, m, s = map(int, match.groups())
        return h * 3600 + m * 60 + s

    # 其次匹配 MM:SS
    match = re.search(r"(\d{1,2}):(\d{2})", timestamp_str)
    if match:
        m, s = map(int, match.groups())
        return m * 60 + s

    return 0


def format_seconds_to_timestamp(seconds):
    """将秒数转换为 [HH:MM:SS] 格式"""
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = int(seconds % 60)
    return f"[{h:02d}:{m:02d}:{s:02d}]"


def wait_for_files_active(file_name):
    print("⏳ 等待云端处理...", end="", flush=True)
    while True:
        file_obj = client.files.get(name=file_name)
        if file_obj.state == "ACTIVE":
            print(" 就绪！")
            return file_obj
        elif file_obj.state == "FAILED":
            raise Exception("云端文件处理失败")
        time.sleep(2)


def save_transcript(data, output_path, raw_json_path):
    # 保存 JSON
    with open(raw_json_path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)

    # 保存可读文本
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(f"=== 📝 摘要 ===\n{data.get('summary', '无')}\n\n")
        f.write("=== 🎙️ 对话详情 ===\n")
        for seg in data.get("segments", []):
            line = f"[{seg.get('timestamp')}] {seg.get('speaker')} ({seg.get('emotion')}): {seg.get('content')}\n"
            f.write(line)


def process_audio_chunk(chunk_path):
    """上传并处理单个音频分片"""
    uploaded_file = None
    try:
        print(f"    ⬆️  上传分片...", end="", flush=True)
        uploaded_file = client.files.upload(file=chunk_path)
        wait_for_files_active(uploaded_file.name)

        print(" 🧠 分析中...", end="", flush=True)
        response = client.models.generate_content(
            model=MODEL_NAME,
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
            print(" ⚠️ API 返回空内容")
            return None

        return json.loads(response.text)

    except Exception as e:
        print(f" ❌ 分片处理错误: {e}")
        return None
    finally:
        if uploaded_file:
            try:
                client.files.delete(name=uploaded_file.name)
                print(" (☁️ 云端文件已清理)", end="", flush=True)
            except:
                pass


def batch_process():
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)
    if not os.path.exists(INPUT_FOLDER):
        return

    # 扫描所有音频格式
    extensions = (".m4a", ".mp3", ".wav", ".flac", ".aac", ".ogg")
    files = [f for f in os.listdir(INPUT_FOLDER) if f.lower().endswith(extensions)]

    print(f"🚀 任务列表: {len(files)} 个文件")

    for index, filename in enumerate(files):
        original_file_path = os.path.join(INPUT_FOLDER, filename)
        file_stem = Path(filename).stem
        output_txt = os.path.join(OUTPUT_FOLDER, f"{file_stem}.txt")
        output_json = os.path.join(OUTPUT_FOLDER, f"{file_stem}.json")

        if os.path.exists(output_txt):
            print(f"⏭️ 跳过已存在: {filename}")
            continue

        print(f"\n[{index+1}/{len(files)}] 📂 处理: {filename}")

        try:
            print(f"  🔄 加载音频中...", end="", flush=True)
            audio = AudioSegment.from_file(original_file_path)
            duration_ms = len(audio)
            duration_min = duration_ms / 1000 / 60
            print(f" 长度: {duration_min:.2f} 分钟")
        except Exception as e:
            print(f"\n❌ 读取音频失败: {e}")
            print("提示: 请确保已安装 ffmpeg。")
            continue

        chunk_length_ms = CHUNK_MINUTES * 60 * 1000
        total_chunks = (duration_ms + chunk_length_ms - 1) // chunk_length_ms

        all_segments = []
        all_summaries = []

        for i in range(total_chunks):
            start_ms = i * chunk_length_ms
            end_ms = min((i + 1) * chunk_length_ms, duration_ms)

            # 导出分片
            chunk = audio[start_ms:end_ms]
            chunk_filename = f"temp_{uuid.uuid4().hex}_chunk_{i}.mp3"
            chunk_path = os.path.join(INPUT_FOLDER, chunk_filename)

            print(
                f"  🔪 分片 [{i+1}/{total_chunks}] ({start_ms//1000}s - {end_ms//1000}s)...",
                end="",
                flush=True,
            )

            try:
                chunk.export(chunk_path, format="mp3", bitrate="128k")

                # 处理分片
                result = process_audio_chunk(chunk_path)

                if result:
                    print(" ✅")
                    # 收集摘要
                    if result.get("summary"):
                        all_summaries.append(f"[分片{i+1}] {result['summary']}")

                    # 处理并收集对话片段
                    for seg in result.get("segments", []):
                        # 调整时间戳
                        original_ts = seg.get("timestamp", "")
                        seconds = parse_timestamp_to_seconds(original_ts)
                        adjusted_seconds = seconds + (start_ms / 1000)
                        seg["timestamp"] = format_seconds_to_timestamp(adjusted_seconds)
                        all_segments.append(seg)
                else:
                    print(" ⚠️ 此分片无结果")

            except Exception as e:
                print(f"\n❌ 分片处理异常: {e}")

            finally:
                # 清理本地分片文件
                if os.path.exists(chunk_path):
                    try:
                        os.remove(chunk_path)
                    except:
                        pass

        # 合并结果
        final_result = {"summary": "\n".join(all_summaries), "segments": all_segments}

        save_transcript(final_result, output_txt, output_json)
        print(f"🎉 文件 {filename} 处理完成！")


if __name__ == "__main__":
    batch_process()
