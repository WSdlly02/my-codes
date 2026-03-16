# audio-process

用于批量音频转写，分为本地 qwen-asr 路线和 Gemini 路线。

## 文件说明

- `trans-local.py`: 批量把音频转为 `wav`，再通过 Docker 调用 `qwen-asr.py` 转写，默认输出到 `transcripts_local/`
- `trans-genai.py`: 使用 Gemini 分片转写目录中的音频，默认输出到 `transcripts_genai/`
- `qwen-asr.py`: 容器内执行的单文件转写入口
- `Dockerfile.qwen-asr:cpu`: 本地构建 qwen-asr 镜像
- `.envrc`: 创建虚拟环境并安装 Python 依赖
- `requirements.txt`: `trans-genai.py` 的 Python 依赖

## 依赖

- 本地转写
  - Docker
  - `ffmpeg`
  - 可运行的 `qwen-asr:cpu` 镜像
- Gemini 转写
  - `GEMINI_API_KEY`
  - `ffmpeg`
  - `python-dotenv`, `google-genai`, `pydub`

## 用法

### 1. 本地 qwen-asr 批量转写

```bash
cd SOPs/audio-process
python trans-local.py /path/to/audio_dir
```

常用参数：

```bash
python trans-local.py /path/to/audio_dir \
  --output-dir transcripts_local \
  --wav-dir .qwen_asr_wavs \
  --keep-wav \
  --force
```

说明：

- 输入目录下的音频会逐个处理
- 脚本会先统一转成 16k 单声道 `wav`
- 然后使用本地现成的 `qwen-asr:cpu` 镜像在容器中运行 `qwen-asr.py`
- 如果本地不存在该镜像，脚本会直接失败，不会自动拉取或构建
- 默认会保留一份 `xxx.txt`，同时额外保存 `xxx-raw-transcription.md`

### 2. Gemini 批量转写

```bash
cd SOPs/audio-process
python trans-genai.py /path/to/audio_dir
```

常用参数：

```bash
python trans-genai.py /path/to/audio_dir \
  --chunk-minutes 10 \
  --model gemini-3-pro-preview \
  --output-dir transcripts_genai \
  --force
```

输出：

- `xxx.txt`: 可读版转写结果
- `xxx.json`: 原始结构化结果
