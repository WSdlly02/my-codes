#!/usr/bin/env python3
"""Convert one lecture atom from source files to an indexed Markdown note."""

from __future__ import annotations

import argparse
import base64
import json
import mimetypes
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


DEFAULT_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"

SYSTEM_PROMPT = """你是国际航运代理课程资料整理助手。你的目标是把课程讲义整理成开卷考试可快速检索的纸质资料。
只依据给定图片和文字提炼，不编造。保留定义、分类、主体关系、业务流程、单证、条款、责任、计算规则、易混点和可能考题。
删除口语寒暄、重复铺垫、无知识价值的转述。输出中文 Markdown。"""

OCR_SYSTEM_PROMPT = """你是课程资料单页 OCR 与保守清洗助手。
你的任务是忠实识别单页图片中的文字、表格和版面信息，形成可追溯的 Markdown 证据层。
不要总结，不要扩写，不要补充课外知识。"""

PAGE_OCR_PROMPT = """请对这一页课程资料做忠实 OCR 和保守清洗。

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

SYNTHESIS_PROMPT = """下面是同一节课逐页 OCR 与保守清洗后的 Markdown。
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


@dataclass
class RenderedPage:
    source_role: str
    source_path: Path
    page_no: int
    image_path: Path


def run(command: list[str]) -> None:
    subprocess.run(command, check=True)


def require_tool(name: str) -> str:
    found = shutil.which(name)
    if not found:
        raise RuntimeError(f"Missing required tool: {name}")
    return found


def read_manifest(path: Path) -> list[dict]:
    with path.open("r", encoding="utf-8") as fh:
        return [json.loads(line) for line in fh if line.strip()]


def find_atom(records: list[dict], selector: str) -> dict:
    matches = [
        record
        for record in records
        if selector == record["atom_id"]
        or selector in record["title"]
        or selector in record["base_name"]
    ]
    if not matches:
        raise SystemExit(f"No atom matched selector: {selector}")
    if len(matches) > 1:
        print("Multiple atoms matched; please use atom_id:", file=sys.stderr)
        for record in matches:
            print(
                f"- {record['atom_id']} {record['chapter']} {record['title']}",
                file=sys.stderr,
            )
        raise SystemExit(2)
    return matches[0]


def convert_docx_to_pdf(source: Path, work_dir: Path) -> Path:
    require_tool("libreoffice")
    out_dir = work_dir / "pdf"
    profile_dir = work_dir / "libreoffice_profile" / source.stem
    out_dir.mkdir(parents=True, exist_ok=True)
    profile_dir.mkdir(parents=True, exist_ok=True)
    expected = out_dir / f"{source.stem}.pdf"
    if not expected.exists():
        run(
            [
                "libreoffice",
                "--headless",
                "--nologo",
                "--nofirststartwizard",
                f"-env:UserInstallation={profile_dir.resolve().as_uri()}",
                "--convert-to",
                "pdf",
                "--outdir",
                str(out_dir),
                str(source),
            ]
        )
    if not expected.exists():
        raise RuntimeError(f"LibreOffice did not create expected PDF: {expected}")
    return expected


def render_pdf_to_images(
    source_pdf: Path,
    work_dir: Path,
    role: str,
    original_source: Path,
    dpi: int,
    max_pages: int | None,
) -> list[RenderedPage]:
    require_tool("pdftoppm")
    image_dir = work_dir / "pages" / source_pdf.stem
    image_dir.mkdir(parents=True, exist_ok=True)
    prefix = image_dir / "page"

    command = ["pdftoppm", "-png", "-r", str(dpi)]
    if max_pages:
        command.extend(["-f", "1", "-l", str(max_pages)])
    command.extend([str(source_pdf), str(prefix)])
    if not list(image_dir.glob("page-*.png")):
        run(command)

    pages: list[RenderedPage] = []
    for index, image in enumerate(sorted(image_dir.glob("page-*.png")), start=1):
        pages.append(RenderedPage(role, original_source, index, image))
    return pages


def render_source_file(
    source: Path,
    role: str,
    work_dir: Path,
    dpi: int,
    max_pages_per_file: int | None,
) -> list[RenderedPage]:
    suffix = source.suffix.lower()
    if suffix == ".pdf":
        pdf = source
    elif suffix == ".docx":
        pdf = convert_docx_to_pdf(source, work_dir)
    else:
        raise ValueError(f"Unsupported source file: {source}")
    return render_pdf_to_images(pdf, work_dir, role, source, dpi, max_pages_per_file)


def image_data_uri(path: Path) -> str:
    mime_type, _ = mimetypes.guess_type(path)
    if not mime_type:
        mime_type = "image/png"
    encoded = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:{mime_type};base64,{encoded}"


def create_client():
    from dotenv import load_dotenv
    from openai import OpenAI

    load_dotenv()
    api_key = os.getenv("DASHSCOPE_API_KEY") or os.getenv("QWEN_API_KEY")
    if not api_key:
        raise RuntimeError("Set DASHSCOPE_API_KEY or QWEN_API_KEY before calling Qwen.")
    return OpenAI(
        api_key=api_key,
        base_url=os.getenv("DASHSCOPE_BASE_URL", DEFAULT_BASE_URL),
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


def page_note_filename(page: RenderedPage, index: int) -> str:
    safe_stem = "".join(
        char if char.isalnum() or char in "-_" else "_"
        for char in page.source_path.stem
    )
    return f"page_{index:04d}_{page.source_role}_{safe_stem}_p{page.page_no:03d}.md"


def call_qwen_page_ocr(
    client, model: str, atom: dict, page: RenderedPage, raw_dir: Path, index: int
) -> str:
    content = [
        {
            "type": "input_text",
            "text": PAGE_OCR_PROMPT.format(
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
            {"role": "system", "content": OCR_SYSTEM_PROMPT},
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


def call_qwen_synthesis(
    client, model: str, atom: dict, page_notes: list[str], raw_dir: Path
) -> str:
    joined = "\n\n--- PAGE BREAK ---\n\n".join(page_notes)
    response = client.responses.create(
        model=model,
        input=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {
                "role": "user",
                "content": SYNTHESIS_PROMPT.format(
                    atom_id=atom["atom_id"], title=atom["title"]
                )
                + "\n\n"
                + joined,
            },
        ],
        temperature=0.1,
        reasoning={"effort": "medium"},
    )
    (raw_dir / "synthesis.json").write_text(
        response.model_dump_json(indent=2, exclude_none=True),
        encoding="utf-8",
    )
    return response_text(response)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "selector", help="atom_id, title substring, or base_name substring."
    )
    parser.add_argument(
        "--manifest",
        default="国际航运代理理论与实务/整理与索引版本/04_检索数据/manifest.jsonl",
        type=Path,
    )
    parser.add_argument("--course-root", default=".", type=Path)
    parser.add_argument(
        "--vision-model",
        default=os.getenv("QWEN_VISION_MODEL", "qwen3.6-35b-a3b"),
        help="Model used for single-page OCR. Default: QWEN_VISION_MODEL or qwen3.6-35b-a3b.",
    )
    parser.add_argument(
        "--synthesis-model",
        default=os.getenv(
            "QWEN_SYNTHESIS_MODEL", os.getenv("QWEN_MODEL", "qwen3.6-plus-2026-04-02")
        ),
        help="Model used for final atom synthesis. Default: QWEN_SYNTHESIS_MODEL/QWEN_MODEL or qwen3.6-plus-2026-04-02.",
    )
    parser.add_argument("--dpi", type=int, default=144)
    parser.add_argument("--max-pages-per-file", type=int)
    parser.add_argument("--max-pages-total", type=int)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--render-only",
        action="store_true",
        help="Render page images and stop before Qwen calls.",
    )
    parser.add_argument(
        "--ocr-only",
        action="store_true",
        help="Run single-page OCR and stop before final atom synthesis.",
    )
    parser.add_argument(
        "--reuse-page-md",
        action="store_true",
        help="Reuse existing per-page Markdown files instead of calling the vision model again.",
    )
    args = parser.parse_args()

    records = read_manifest(args.manifest)
    atom = find_atom(records, args.selector)
    output_md = args.course_root / atom["output_md"]
    cache_dir = (
        args.course_root
        / "国际航运代理理论与实务/整理与索引版本/99_过程缓存"
        / atom["atom_id"]
    )

    print(f"Atom: {atom['atom_id']} {atom['title']}")
    print(f"Chapter: {atom['chapter']}")
    for item in atom["source_files"]:
        print(f"- {item['role']}: {item['path']}")

    if args.dry_run:
        print(f"Would render pages into: {cache_dir}")
        print(f"Would write Markdown to: {output_md}")
        return

    rendered_pages: list[RenderedPage] = []
    for source in atom["source_files"]:
        path = args.course_root / source["path"]
        rendered_pages.extend(
            render_source_file(
                path, source["role"], cache_dir, args.dpi, args.max_pages_per_file
            )
        )

    if args.max_pages_total:
        rendered_pages = rendered_pages[: args.max_pages_total]
    if not rendered_pages:
        raise SystemExit("No pages rendered.")
    if args.render_only:
        print(f"Rendered {len(rendered_pages)} pages into: {cache_dir / 'pages'}")
        return

    client = create_client()
    raw_dir = cache_dir / "qwen_raw_outputs"
    page_md_dir = cache_dir / "page_md"
    page_md_dir.mkdir(parents=True, exist_ok=True)
    page_notes: list[str] = []
    for index, page in enumerate(rendered_pages, start=1):
        page_md_path = page_md_dir / page_note_filename(page, index)
        if args.reuse_page_md and page_md_path.exists():
            print(
                f"Reusing page OCR {index}/{len(rendered_pages)}: {page_md_path.name}"
            )
            page_note = page_md_path.read_text(encoding="utf-8")
        else:
            print(
                f"Calling Qwen page OCR with {args.vision_model}: {index}/{len(rendered_pages)}: {page.source_role} {page.source_path.name} p.{page.page_no}"
            )
            page_note = call_qwen_page_ocr(
                client, args.vision_model, atom, page, raw_dir, index
            )
            page_md_path.write_text(page_note.rstrip() + "\n", encoding="utf-8")
        page_notes.append(page_note)

    if args.ocr_only:
        print(f"Wrote {len(page_notes)} page OCR Markdown files to: {page_md_dir}")
        return

    print(f"Calling Qwen synthesis with {args.synthesis_model}")
    final_md = call_qwen_synthesis(
        client, args.synthesis_model, atom, page_notes, raw_dir
    )
    output_md.parent.mkdir(parents=True, exist_ok=True)
    output_md.write_text(final_md.rstrip() + "\n", encoding="utf-8")
    print(f"Wrote {output_md}")


if __name__ == "__main__":
    main()
