import sys
from pathlib import Path
import os
import torch

# ── 线程配置：物理核心推理，最小化跨操作同步 ──────────────────
cpu_count = os.cpu_count() or 4
physical_cores = max(cpu_count // 2, 1)
torch.set_num_threads(physical_cores)
torch.set_num_interop_threads(2)

from qwen_asr import Qwen3ASRModel


def build_output_path(input_path: Path) -> Path:
    return input_path.with_name(f"{input_path.stem}-raw-transcription.md")


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: python3 qwen-asr.py <audio_path>", file=sys.stderr)
        return 2

    input_path = Path(sys.argv[1]).resolve()
    if not input_path.is_file():
        print(f"input file not found: {input_path}", file=sys.stderr)
        return 2

    model = Qwen3ASRModel.from_pretrained(
        "Qwen/Qwen3-ASR-1.7B",
        dtype=torch.bfloat16,
        device_map="cpu",
        max_inference_batch_size=1,
        max_new_tokens=8192,
    )

    with torch.inference_mode():
        results = model.transcribe(
            audio=str(input_path),
            language=None,
        )

    text = results[0].text.strip()
    out_path = build_output_path(input_path)
    out_path.write_text(text + "\n", encoding="utf-8")
    print(out_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
