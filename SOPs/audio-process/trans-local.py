#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import subprocess
from pathlib import Path

SUPPORTED_EXTENSIONS = {".mp3", ".wav", ".m4a", ".mp4", ".flac", ".mkv", ".aac", ".ogg"}
IMAGE_NAME = "qwen-asr:cpu"
REMOTE_SCRIPT_PATH = "/workspace/qwen-asr.py"
REMOTE_AUDIO_DIR = "/data"
REMOTE_OUTPUT_DIR = "/output"


def parse_args():
    parser = argparse.ArgumentParser(
        description="使用本地 qwen-asr 批量转写音频文件，并输出 txt。"
    )
    parser.add_argument("input_dir", help="音频目录")
    parser.add_argument(
        "-o",
        "--output-dir",
        default="transcripts_local",
        help="输出目录名或路径，默认在输入目录下创建 transcripts_local",
    )
    parser.add_argument(
        "--wav-dir",
        default=".qwen_asr_wavs",
        help="中间 wav 文件目录名或路径，默认在输入目录下创建 .qwen_asr_wavs",
    )
    parser.add_argument(
        "--keep-wav",
        action="store_true",
        help="保留中间 wav 文件，默认处理完成后删除",
    )
    parser.add_argument(
        "--ffmpeg-bin",
        default="ffmpeg",
        help="ffmpeg 可执行文件名，默认 ffmpeg",
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

    script_dir = Path(__file__).resolve().parent
    output_dir = Path(args.output_dir).expanduser()
    if not output_dir.is_absolute():
        output_dir = input_dir / output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    wav_dir = Path(args.wav_dir).expanduser()
    if not wav_dir.is_absolute():
        wav_dir = input_dir / wav_dir
    wav_dir.mkdir(parents=True, exist_ok=True)

    qwen_asr_script = script_dir / "qwen-asr.py"
    dockerfile_path = script_dir / f"Dockerfile.{IMAGE_NAME}"
    if not qwen_asr_script.is_file():
        print(f"❌ 错误: 未找到 {qwen_asr_script}")
        return 1
    if not dockerfile_path.is_file():
        print(f"❌ 错误: 未找到 {dockerfile_path}")
        return 1

    files = sorted(
        path
        for path in input_dir.iterdir()
        if path.suffix.lower() in SUPPORTED_EXTENSIONS
    )
    if not files:
        print(f"📂 {input_dir} 中没有找到支持的音频文件。")
        return 1

    print(f"📋 任务列表: 共 {len(files)} 个文件")
    print("=" * 60)

    try:
        ensure_docker_image_exists()
    except Exception as exc:
        print(f"❌ Docker 镜像检查失败: {exc}")
        return 1

    failures = 0
    for index, audio_path in enumerate(files, start=1):
        file_stem = audio_path.stem
        final_txt_path = output_dir / f"{file_stem}.txt"
        wav_path = wav_dir / f"{file_stem}.wav"
        raw_output_path = output_dir / f"{file_stem}-raw-transcription.md"

        if final_txt_path.exists() and not args.force:
            print(f"[{index}/{len(files)}] ⏭️ 跳过 (已存在): {audio_path.name}")
            continue

        print(f"\n[{index}/{len(files)}] 🎙️ 正在处理: {audio_path.name}")
        print("-" * 30)

        try:
            convert_to_wav(audio_path, wav_path, args.ffmpeg_bin)
            generated_raw_path = run_qwen_asr_container(
                script_dir=script_dir,
                wav_dir=wav_dir,
                output_dir=output_dir,
                wav_path=wav_path,
            )

            final_txt_path.write_text(
                generated_raw_path.read_text(encoding="utf-8").strip() + "\n",
                encoding="utf-8",
            )

            if generated_raw_path != raw_output_path and generated_raw_path.exists():
                generated_raw_path.rename(raw_output_path)

            print(f"✅ 处理完成: {audio_path.name} -> {final_txt_path.name}")
        except subprocess.CalledProcessError as exc:
            failures += 1
            print(f"❌ 处理失败 {audio_path.name}: {exc}")
        except Exception as exc:
            failures += 1
            print(f"❌ 未知错误 {audio_path.name}: {exc}")
        finally:
            if not args.keep_wav:
                wav_path.unlink(missing_ok=True)

    print("\n🎉 所有文件处理完毕。")
    return 1 if failures else 0


def ensure_docker_image_exists() -> None:
    inspect_cmd = ["docker", "image", "inspect", IMAGE_NAME]
    inspect_result = subprocess.run(
        inspect_cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        check=False,
    )
    if inspect_result.returncode == 0:
        return
    raise RuntimeError(f"本地未找到 Docker 镜像 {IMAGE_NAME}")


def convert_to_wav(audio_path: Path, wav_path: Path, ffmpeg_bin: str) -> None:
    wav_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        ffmpeg_bin,
        "-y",
        "-i",
        str(audio_path),
        "-vn",
        "-ac",
        "1",
        "-ar",
        "16000",
        str(wav_path),
    ]
    subprocess.run(
        cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )


def run_qwen_asr_container(
    script_dir: Path,
    wav_dir: Path,
    output_dir: Path,
    wav_path: Path,
) -> Path:
    cache_dir = Path.home() / ".cache" / "huggingface"
    cache_dir.mkdir(parents=True, exist_ok=True)

    remote_wav_path = Path(REMOTE_AUDIO_DIR) / wav_path.name
    cmd = [
        "docker",
        "run",
        "--rm",
        "-v",
        f"{cache_dir}:/root/.cache/huggingface",
        "-v",
        f"{script_dir}:/workspace",
        "-v",
        f"{wav_dir}:{REMOTE_AUDIO_DIR}",
        "-v",
        f"{output_dir}:{REMOTE_OUTPUT_DIR}",
        "-w",
        REMOTE_OUTPUT_DIR,
        IMAGE_NAME,
        REMOTE_SCRIPT_PATH,
        str(remote_wav_path),
    ]
    result = subprocess.run(cmd, check=True, capture_output=True, text=True)

    output_lines = [line.strip() for line in result.stdout.splitlines() if line.strip()]
    if not output_lines:
        raise RuntimeError("qwen-asr 容器未返回输出文件路径")

    container_output_path = Path(output_lines[-1])
    generated_name = container_output_path.name
    generated_raw_path = wav_dir / generated_name
    if not generated_raw_path.exists():
        alt_path = output_dir / generated_name
        if alt_path.exists():
            return alt_path
        raise FileNotFoundError(f"未找到 qwen-asr 输出文件: {generated_name}")
    return generated_raw_path


if __name__ == "__main__":
    raise SystemExit(main())
