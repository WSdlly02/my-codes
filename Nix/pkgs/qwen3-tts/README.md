# Qwen3-TTS on the RX 6700 XT

From the repository root on x86_64-linux:

```sh
nix build .#qwen-tts --dry-run
nix develop .#qwen-tts
```

The shell provides Python 3.12, Qwen's Python API and CLI, PyTorch and torchaudio built with ROCm, and SoX. The package uses the repository’s locked Nixpkgs Python dependencies with ROCm enabled. Its build patches the Qwen model for Transformers 5 and the demo for Gradio 6, and writes the selected Transformers and Accelerate versions into the Python package metadata. Building missing ROCm binaries, especially PyTorch, can take substantial time and disk space. Model weights are fetched on first use into the Hugging Face cache; you can also pass a local model directory.

## Check the GPU before downloading weights

The RX 6700 XT reports gfx1031. The shell targets gfx1030 and sets `HSA_OVERRIDE_GFX_VERSION=10.3.0` for compatibility. This is a workaround that needs verification on your driver and card. The host user needs access to `/dev/kfd` and the GPU render node. If the 9700X integrated GPU is also visible, use `rocminfo` to find the discrete GPU's UUID and set `ROCR_VISIBLE_DEVICES` to that UUID before starting Python.

```sh
python - <<'PY'
import torch
from qwen_tts import Qwen3TTSModel

assert torch.version.hip and torch.cuda.is_available(), "ROCm device unavailable"
print(torch.version.hip, torch.cuda.get_device_name(0))
x = torch.ones((64, 64), device="cuda:0", dtype=torch.float16)
assert (x @ x)[0, 0].item() == 64
PY
```

## Synthesize a short sample

```python
import soundfile as sf
import torch
from qwen_tts import Qwen3TTSModel

model = Qwen3TTSModel.from_pretrained(
    "Qwen/Qwen3-TTS-12Hz-0.6B-CustomVoice",
    device_map="cuda:0",
    dtype=torch.float16,
    attn_implementation="eager",
)
wavs, rate = model.generate_custom_voice(
    text="今天，上海港迎来了第一艘集装箱船。",
    language="Chinese",
    speaker="Vivian",
)
sf.write("sample.wav", wavs[0], rate)
```

To use the local web interface in the same shell:

```sh
qwen-tts-demo Qwen/Qwen3-TTS-12Hz-0.6B-CustomVoice \
  --device cuda:0 --dtype float16 --no-flash-attn --concurrency 1 --ip 127.0.0.1
```
