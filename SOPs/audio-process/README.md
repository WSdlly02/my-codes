# audio-process

用于批量音频转写，分为本地 Whisper 路线和 Gemini 路线。

## 文件说明

- `trans-local.py`: 调用 `whisper.sh` 批量转写目录中的音频，默认输出到 `transcripts_local/`
- `trans-genai.py`: 使用 Gemini 分片转写目录中的音频，默认输出到 `transcripts_genai/`
- `whisper.sh`: 通过 Docker 启动 Whisper 运行环境
- `Dockerfile.openai-whisper:*`: 本地构建 Whisper 镜像
- `.envrc`: 创建虚拟环境并安装 Python 依赖
- `requirements.txt`: `trans-genai.py` 的 Python 依赖

## 依赖

- 本地转写
  - Docker
  - 可运行的 Whisper 镜像
- Gemini 转写
  - `GEMINI_API_KEY`
  - `ffmpeg`
  - `python-dotenv`, `google-genai`, `pydub`

## 用法

### 1. 本地 Whisper 批量转写

```bash
cd SOPs/audio-process
python trans-local.py /path/to/audio_dir
```

常用参数：

```bash
python trans-local.py /path/to/audio_dir \
  --model large-v3-turbo \
  --language Chinese \
  --output-dir transcripts_local \
  --force
```

说明：

- 输入目录下的音频会逐个处理
- 默认读取 `whisper.sh`
- 当输出格式为 `srt` 时，会自动整理成 `txt`

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
