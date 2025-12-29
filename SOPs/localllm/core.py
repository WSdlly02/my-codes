#! /usr/bin/env python3
# -*- coding: utf-8 -*-
import json
import requests

DEFAULT_PROMPT_MAP = {
    "t": "Extract the text in the image.",
    "md": "Extract the text in the image and format it as Markdown, preserving the structure and layout.",
    "f": "Extract the mathematical formulas from the image and provide them in LaTeX format.",
    "table": "Extract the table from the image and format it as a Markdown table.",
    "json": "Extract the information from the image and format it as a JSON object.",
    "desc": "Describe the image in detail.",
}


def generate_ocr_stream(
    base64_image, prompt_style="t", custom_prompt="", model="qwen3-vl:8b"
):
    """
    生成 OCR 识别结果的流
    Yields:
        str: 识别到的文本片段
    """
    selected_prompt = (
        DEFAULT_PROMPT_MAP.get(prompt_style) if not custom_prompt else custom_prompt
    )

    # 使用 Ollama 原生 API 以获得更好的兼容性
    url = "http://localhost:11434/api/generate"
    payload = {
        "model": model,
        "prompt": f"\n{selected_prompt}",
        "images": [base64_image],
        "think": False,
        "stream": True,
        "options": {"temperature": 0},
    }

    try:
        response = requests.post(url, json=payload, stream=True)
        response.raise_for_status()

        for line in response.iter_lines():
            if line:
                chunk = json.loads(line)
                content = chunk.get("response", "")
                yield content
                if chunk.get("done"):
                    break
    except Exception as e:
        raise e
