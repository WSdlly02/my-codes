# SOPs

这里收集了常用标准操作脚本，按处理对象分目录维护。

## 目录概览

- `audio-process/`: 音频批量转写
- `image-process/`: 图片 OCR
- `pdf-process/`: PDF 导图、OCR、图片合并

## 通用约定

- 每个子目录的 `.envrc` 会创建本地虚拟环境并安装 `requirements.txt`
- 优先通过命令行参数传入输入目录、输出目录和模型名，避免再改源码中的硬编码路径
- 默认输出会写到输入目录下的子目录中，便于就地处理

## 快速开始

### 音频转写

```bash
cd SOPs/audio-process
python trans-local.py /path/to/audio_dir
python trans-genai.py /path/to/audio_dir
```

### 图片 OCR

```bash
cd SOPs/image-process
python ocr-local-once.py /path/to/image.png
python ocr-local.py /path/to/image_dir
```

### PDF OCR

```bash
cd SOPs/pdf-process
python export-imgs.py /path/to/file.pdf
python ocr-local.py /path/to/pdf_dir
python ocr-genai.py /path/to/pdf_dir
```

## 说明

- 详细参数请直接执行各脚本的 `--help`
- 各子目录都有单独 `README.md`，记录依赖和常见命令
