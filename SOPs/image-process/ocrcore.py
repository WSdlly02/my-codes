#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import json
from typing import Iterator

import requests


OCR_STYLE_MAP = {
    "text": """
Transcribe all legible text exactly as it appears in the image.
Preserve line breaks, spacing, punctuation, and casing.
Do not add any explanations, conversational filler, or markdown unless it is literally present in the image.
Return ONLY the raw text.
""",
    "markdown": """
Transcribe the content into Markdown with minimal transformation.
Only use headings, lists, bold/italic, links, and code blocks if they are clearly indicated in the image.
Preserve the original reading order and line breaks as much as possible.
If some text is illegible, keep its position and use '□' as a placeholder.
Return ONLY the Markdown content (no code fences).
""",
    "latex": """
Transcribe the content, using LaTeX ONLY for mathematical expressions.
Use $...$ for inline math and $$...$$ for displayed equations when clearly indicated.
Keep surrounding non-math text as plain text and preserve line breaks.
If any symbol/character is illegible, use '□' in its place (do not guess).
Return ONLY the mixed plain text + LaTeX content (no code fences).
""",
    "table": """
If the image contains a table, transcribe it using Markdown table syntax.
Preserve row order and keep cell text exactly as seen.
Only create a table when column boundaries are clear; otherwise, output the rows as plain text lines in order.
If any cell text is illegible, use '□' as a placeholder (do not guess).
Return ONLY the content (no code fences).
""",
    "json": """
Extract ONLY the structured fields that are explicitly visible in the image (receipts, forms, or key-value pairs).
Output a valid JSON object (double quotes, no trailing commas). Use lowerCamelCase for keys.
Do NOT invent keys or values. Do NOT infer missing fields.
If a value is illegible or uncertain, use null.
Return ONLY the JSON string (no markdown, no code fences).
""",
    "desc": """
Describe the image in detail: layout, main objects, colors, and any visible text (quote text verbatim when possible).
If something is unclear, say it is unclear or use cautious language (e.g., "possibly", "appears to").
Do not fabricate specific details that are not visible.
Return ONLY the description.
""",
}

OCR_MODE_MAP = {
    "strict": """
You are an OCR engine. Transcribe content from the image faithfully.
Rules:
- Do NOT invent, guess, or auto-correct.
- Preserve line breaks, spacing, punctuation, and casing as seen.
- If something is illegible, output '□' (use one '□' for unknown length).
- Output ONLY the requested content. No explanations. No code fences.
""",
    "enhance": """You are an OCR assistant focused on producing clean, usable text.
Rules:
- Ignore watermark/overlay text that is clearly non-content (repeated, semi-transparent, crossing the page).
- You may repair obvious OCR errors and reconnect broken lines.
- If you infer missing/unclear text, wrap the inferred part in ⟦ ⟧.
- For JSON output: NEVER use ⟦ ⟧ inside JSON; use null for uncertain values and do not infer missing fields.
- Do NOT add new information beyond what can be reasonably inferred from visible context.
- Output ONLY the requested content. No explanations. No code fences.
""",
}

# Backward-compatible aliases used by existing scripts.
OCRSTYLE = OCR_STYLE_MAP
OCRMODE = OCR_MODE_MAP
DEFAULT_PROMPT_MAP = {
    "t": "text",
    "text": "text",
    "md": "markdown",
    "markdown": "markdown",
    "f": "latex",
    "latex": "latex",
    "table": "table",
    "json": "json",
    "desc": "desc",
}

DEFAULT_MODEL = "qwen3-vl:8b-instruct"
DEFAULT_OLLAMA_URL = "http://localhost:11434/api/generate"


def normalize_style(style: str) -> str:
    normalized = DEFAULT_PROMPT_MAP.get(style, style)
    if normalized not in OCR_STYLE_MAP:
        choices = ", ".join(sorted(DEFAULT_PROMPT_MAP))
        raise ValueError(f"不支持的 OCR 风格: {style}，可选值: {choices}")
    return normalized


def generate_ocr_stream_local(
    base64_image: str,
    ocr_style: str = "text",
    ocr_mode: str = "strict",
    custom_style: str = "",
    custom_mode: str = "",
    model: str = DEFAULT_MODEL,
    ollama_url: str = DEFAULT_OLLAMA_URL,
    timeout: int = 300,
) -> Iterator[str]:
    final_style = custom_style or OCR_STYLE_MAP[normalize_style(ocr_style)]
    final_mode = custom_mode or OCR_MODE_MAP.get(ocr_mode, OCR_MODE_MAP["strict"])

    payload = {
        "model": model,
        "system": final_mode,
        "prompt": final_style,
        "images": [base64_image],
        "think": False,
        "stream": True,
        "options": {
            "temperature": 0.2,
            "num_ctx": 8192,
            "num_predict": 4096,
            "repeat_penalty": 1.1,
            "top_k": 20,
            "top_p": 0.8,
            "min_p": 0,
        },
    }

    try:
        response = requests.post(ollama_url, json=payload, stream=True, timeout=timeout)
        response.raise_for_status()

        for line in response.iter_lines():
            if not line:
                continue
            chunk = json.loads(line)
            content = chunk.get("response", "")
            if content:
                yield content
            if chunk.get("done"):
                break
    except requests.RequestException as exc:
        raise RuntimeError(f"Ollama 请求失败: {exc}") from exc
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"Ollama 返回了无法解析的流式数据: {exc}") from exc


def generate_ocr_stream_genai(*args, **kwargs):
    raise NotImplementedError("尚未实现 Gemini OCR 流式适配。")
