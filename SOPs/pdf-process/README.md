# pdf-process

用于 PDF 导出图片、图片合并 PDF，以及本地 / Gemini OCR。

## 文件说明

- `export-imgs.py`: 将单个 PDF 导出为图片序列
- `ocr-local.py`: 批量处理目录中的 PDF，先转图片，再调用 `image-process/ocr-local.py`
- `ocr-genai.py`: 使用 Gemini 直接把 PDF 提取成 Markdown
- `merge-imgs.py`: 从分页图片 URL 批量下载图片并合并成 PDF
- `.envrc`: 创建虚拟环境并安装依赖
- `requirements.txt`: Python 依赖

## 依赖

- 本地 OCR 路线
  - `PyMuPDF`
  - `Pillow`
  - `image-process` 目录可用的本地 OCR 环境
- Gemini 路线
  - `GEMINI_API_KEY`
  - `google-genai`

## 用法

### 1. 单个 PDF 导出为图片

```bash
cd SOPs/pdf-process
python export-imgs.py /path/to/file.pdf
```

常用参数：

```bash
python export-imgs.py /path/to/file.pdf \
  --dpi 300 \
  --format png \
  --output /path/to/output_dir
```

### 2. 目录批量 PDF 本地 OCR

```bash
cd SOPs/pdf-process
python ocr-local.py /path/to/pdf_dir
```

常用参数：

```bash
python ocr-local.py /path/to/pdf_dir \
  --dpi 300 \
  --prompt-style markdown \
  --output-root /path/to/output_root \
  --force
```

输出结构默认类似：

```text
pdf_dir/
  demo.pdf
  demo/
    demo_page_001.png
    ...
    ocr_results.md
```

### 3. 目录批量 PDF Gemini OCR

```bash
cd SOPs/pdf-process
python ocr-genai.py /path/to/pdf_dir
```

常用参数：

```bash
python ocr-genai.py /path/to/pdf_dir \
  --model gemini-3-flash-preview \
  --output-dir genai_ocr_outputs \
  --force
```
