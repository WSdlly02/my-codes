# image-process

用于单张图片 OCR 和目录批量 OCR，当前默认走本地 Ollama 多模态模型。

## 文件说明

- `ocrcore.py`: OCR prompt、风格映射和 Ollama 请求封装
- `ocr-local-once.py`: 识别单张图片；不传路径时尝试从剪贴板读取
- `ocr-local.py`: 批量识别目录中的图片，并汇总到 Markdown
- `.envrc`: 创建虚拟环境并安装依赖
- `requirements.txt`: Python 依赖

## 依赖

- Ollama 服务，默认地址 `http://localhost:11434`
- 已拉取的视觉模型，默认 `qwen3-vl:8b-instruct`
- `requests`
- 如果要从剪贴板读写，还需要 `wl-clipboard`

## 用法

### 1. 单张图片 OCR

```bash
cd SOPs/image-process
python ocr-local-once.py /path/to/image.png
```

常用参数：

```bash
python ocr-local-once.py /path/to/image.png \
  --prompt-style markdown \
  --model qwen3-vl:8b-instruct
```

如果省略图片路径，脚本会尝试从剪贴板读取图片或文件引用。

### 2. 批量 OCR

```bash
cd SOPs/image-process
python ocr-local.py /path/to/image_dir
```

常用参数：

```bash
python ocr-local.py /path/to/image_dir \
  --prompt-style text \
  --output ocr_results.md \
  --recursive
```

## 支持的 OCR 风格

- `text` 或 `t`: 尽量原样转文本
- `markdown` 或 `md`: 转成 Markdown
- `latex` 或 `f`: 公式优先，数学内容用 LaTeX
- `table`: 表格优先
- `json`: 表单/票据结构化抽取
- `desc`: 图片详细描述
