#!/usr/bin/env python3
"""LLM and OCR API calls used by atom conversion scripts."""

from __future__ import annotations

import base64
import json
import mimetypes
import os
import time
from pathlib import Path

import requests
from dotenv import load_dotenv
from openai import OpenAI


QWEN_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"
PADDLE_LAYOUT_API_URL = "https://jbja57rcmcgas5s7.aistudio-app.com/layout-parsing"

QWEN_SYNTHESIS_SYSTEM_PROMPT = """你是国际航运代理课程资料整理助手。你的目标是把课程讲义整理成开卷考试可快速检索的纸质资料。
只依据给定图片和文字提炼，不编造。保留定义、分类、主体关系、业务流程、单证、条款、责任、计算规则、易混点和可能考题。
删除口语寒暄、重复铺垫、无知识价值的转述。输出中文 Markdown。"""

QWEN_OCR_SYSTEM_PROMPT = """你是课程资料单页 OCR 与保守清洗助手。
你的任务是忠实识别单页图片中的文字、表格和版面信息，形成可追溯的 Markdown 证据层。
不要总结，不要扩写，不要补充课外知识。"""

QWEN_PAGE_OCR_PROMPT = """请对这一页课程资料做忠实 OCR 和保守清洗。

页面信息：
- 课程：{course}
- 章节：{chapter}
- atom：{atom_id} {title}
- 来源角色：{source_role}
- 来源文件：{source_file}
- 渲染页码：p.{page_no}

输出结构：
---
source_role: {source_role}
source_file: {source_file}
page_no: {page_no}
---

## OCR正文

## 表格/流程/图示

## 低置信度与疑似错字

规则：
- 尽量忠实保留原页文字、标题层级、列表、编号和表格。
- 可以去除明显口头废话、页眉页脚、重复水印、无意义装饰。
- 可以做保守纠错，例如明显的 OCR 错字、断行合并、标点修复；不确定必须保留原样并在“低置信度与疑似错字”中说明。
- 图片、流程图、表格不能看清时，写“图像不清：...”，不要猜。
- 不要在本阶段提炼考点，不要合并其他页面信息。"""

QWEN_SYNTHESIS_FROM_PAGE_MD_PROMPT = """下面是同一节课逐页 OCR 与保守清洗后的 Markdown。
这些 page_md 只是 OCR 草稿和中间缓存，不是最终资料的一部分，最终讲义不能引用它们。
请把这些草稿重建为一份可打印、自足、不可再下钻查询的 atom Markdown。

最终结构必须为：
# {atom_id} {title}

## 0. 本讲速查索引
| 查什么 | 直接答案 | 所在小节 |

## 1. 本讲知识地图

## 2. 核心概念与定义

## 3. 主体关系与业务边界

## 4. 业务范围、流程与操作规则

## 5. 单证、条款、法律依据与责任

## 6. 案例、异常情形与处置方法

## 7. 易混淆点与辨析

## 8. 高频考题与答题要点

## 9. 关键词索引
| 关键词 | 含义 | 关联考点 |

## 与其他章节的关联

约束：
- 最终 Markdown 是本讲最小查询单元。读者打印后只看这份文件，应能回答本讲相关问题。
- 不要引用 page_md、缓存路径、原始文件路径或“见某页 OCR”。page_md 只能作为重写材料，不能作为外部引用。
- 可以保留来源页码作为括号内的弱提示，例如“据原文 p.5”，但页码不能替代正文内容。
- 信息密度要高：定义、分类、主体、流程、单证、条款、责任、案例、易混点、考题都要展开到可直接作答。
- 不是逐字稿。删除口头废话、重复铺垫、寒暄和明显无意义内容。
- 不是短摘要。不要把关键案例、规则或定义压成一句话；必要处用表格、步骤、判断规则呈现。
- 基于逐页 OCR 内容合并重复点，保留互相补充的细节。对导读、PPT、原文中重复的信息只保留最清晰版本。
- 不要写学习建议，不要写泛泛总结。
- 不确定内容必须标注“不确定”；对 OCR 疑似错字可保守纠正，并在正文中使用纠正后的术语。
- 输出只包含最终 atom Markdown，不要解释你的处理过程。"""

QWEN_SYNTHESIS_FROM_PADDLE_MD_PROMPT = """下面是同一节课的 PaddleOCR layout-parsing Markdown。
这些 OCR Markdown 是中间缓存，不是最终资料的一部分，最终讲义不能引用它们。
请把这些材料重建为一份可打印、自足、不可再下钻查询的 atom Markdown。

重要背景：
- PaddleOCR 是纯 OCR / 版面解析工具，不负责课程知识总结。
- OCR Markdown 可能包含页眉页脚、水印、换行错误、重复标题、表格错位、图片占位或识别残缺。
- 你需要基于 OCR 内容做知识整理，但不能补充输入材料之外的事实。

最终结构必须为：
# {atom_id} {title}

## 0. 本讲速查索引
| 查什么 | 直接答案 | 所在小节 |

## 1. 本讲知识地图

## 2. 核心概念与定义

## 3. 主体关系与业务边界

## 4. 业务范围、流程与操作规则

## 5. 单证、条款、法律依据与责任

## 6. 案例、异常情形与处置方法

## 7. 易混淆点与辨析

## 8. 高频考题与答题要点

## 9. 关键词索引
| 关键词 | 含义 | 关联考点 |

## 与其他章节的关联

约束：
- 最终 Markdown 是本讲最小查询单元。读者打印后只看这份文件，应能回答本讲相关问题。
- 不要引用 OCR 缓存路径、PaddleOCR、原始文件路径或“见 OCR Markdown”。
- 可以保留来源页码作为括号内的弱提示，例如“据 slides p.5”，但页码不能替代正文内容。
- 信息密度要高：定义、分类、主体、流程、单证、条款、责任、案例、易混点、考题都要展开到可直接作答。
- 不是逐字稿。删除口头废话、重复铺垫、寒暄、页眉页脚、水印和明显无意义内容。
- 不是短摘要。不要把关键案例、规则或定义压成一句话；必要处用表格、步骤、判断规则呈现。
- 基于 OCR 内容合并重复点，保留互相补充的细节。对导读、PPT、原文中重复的信息只保留最清晰版本。
- 对疑似 OCR 错字可以保守纠正，并在正文中使用纠正后的术语；不确定内容必须标注“不确定”。
- 如果 OCR 出现明显重复段落，只保留一次。
- 输出只包含最终 atom Markdown，不要解释你的处理过程。
"""


def create_qwen_client() -> OpenAI:
    load_dotenv()
    api_key = os.getenv("DASHSCOPE_API_KEY") or os.getenv("QWEN_API_KEY")
    if not api_key:
        raise RuntimeError("Set DASHSCOPE_API_KEY or QWEN_API_KEY before calling Qwen.")
    return OpenAI(
        api_key=api_key,
        base_url=os.getenv("DASHSCOPE_BASE_URL", QWEN_BASE_URL),
    )


def response_text(response) -> str:
    text = getattr(response, "output_text", None)
    if text:
        return text
    parts: list[str] = []
    for item in getattr(response, "output", []) or []:
        if getattr(item, "type", None) != "message":
            continue
        for content in getattr(item, "content", []) or []:
            if getattr(content, "type", None) == "output_text":
                parts.append(content.text)
    return "\n".join(parts).strip()


def image_data_uri(path: Path) -> str:
    mime_type, _ = mimetypes.guess_type(path)
    if not mime_type:
        mime_type = "image/png"
    encoded = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:{mime_type};base64,{encoded}"


def call_qwen_page_ocr(
    client: OpenAI, model: str, atom: dict, page, raw_dir: Path, index: int
) -> str:
    content = [
        {
            "type": "input_text",
            "text": QWEN_PAGE_OCR_PROMPT.format(
                course=atom["course"],
                chapter=atom["chapter"],
                atom_id=atom["atom_id"],
                title=atom["title"],
                source_role=page.source_role,
                source_file=page.source_path.name,
                page_no=page.page_no,
            ),
        },
        {"type": "input_image", "image_url": image_data_uri(page.image_path)},
    ]
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": QWEN_OCR_SYSTEM_PROMPT},
            {"role": "user", "content": content},
        ],
        temperature=0,
        reasoning={"effort": "low"},
    )
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / f"page_{index:04d}.json").write_text(
        response.model_dump_json(indent=2, exclude_none=True),
        encoding="utf-8",
    )
    return response_text(response)


def call_qwen_synthesis_from_page_md(
    client: OpenAI, model: str, atom: dict, page_notes: list[str], raw_dir: Path
) -> str:
    joined = "\n\n--- PAGE BREAK ---\n\n".join(page_notes)
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": QWEN_SYNTHESIS_SYSTEM_PROMPT},
            {
                "role": "user",
                "content": QWEN_SYNTHESIS_FROM_PAGE_MD_PROMPT.format(
                    atom_id=atom["atom_id"], title=atom["title"]
                )
                + "\n\n"
                + joined,
            },
        ],
        temperature=0.1,
        reasoning={"effort": "medium"},
    )
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "synthesis.json").write_text(
        response.model_dump_json(indent=2, exclude_none=True),
        encoding="utf-8",
    )
    return response_text(response)


def call_qwen_synthesis_from_paddle_md(
    client: OpenAI, model: str, atom: dict, page_notes: list[str], raw_dir: Path
) -> str:
    joined = "\n\n--- OCR MARKDOWN BREAK ---\n\n".join(page_notes)
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": QWEN_SYNTHESIS_SYSTEM_PROMPT},
            {
                "role": "user",
                "content": QWEN_SYNTHESIS_FROM_PADDLE_MD_PROMPT.format(
                    atom_id=atom["atom_id"], title=atom["title"]
                )
                + "\n\n"
                + joined,
            },
        ],
        temperature=0.1,
        reasoning={"effort": "medium"},
    )
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "synthesis_v2.json").write_text(
        response.model_dump_json(indent=2, exclude_none=True),
        encoding="utf-8",
    )
    return response_text(response)


def create_paddle_payload(path: Path, file_type: int) -> dict:
    encoded = base64.b64encode(path.read_bytes()).decode("ascii")
    return {
        "file": encoded,
        "fileType": file_type,
        "useDocOrientationClassify": False,
        "useDocUnwarping": False,
        "useChartRecognition": False,
    }


def call_paddle_layout(
    token: str,
    path: Path,
    file_type: int,
    timeout: int,
    retries: int,
) -> dict:
    headers = {
        "Authorization": f"token {token}",
        "Content-Type": "application/json",
    }
    payload = create_paddle_payload(path, file_type)
    last_error: Exception | None = None
    for attempt in range(1, retries + 2):
        try:
            response = requests.post(
                PADDLE_LAYOUT_API_URL, json=payload, headers=headers, timeout=timeout
            )
            if response.status_code == 200:
                data = response.json()
                if "result" not in data:
                    raise RuntimeError(f"Paddle response missing result: {data}")
                return data["result"]
            raise RuntimeError(
                f"Paddle HTTP {response.status_code}: {response.text[:500]}"
            )
        except Exception as exc:  # noqa: BLE001
            last_error = exc
            if attempt > retries:
                break
            time.sleep(min(2**attempt, 10))
    raise RuntimeError(f"Paddle OCR failed for {path}: {last_error}") from last_error
