#!/usr/bin/env python3
import os
import re
import subprocess
import time
from pathlib import Path
from typing import Dict, List, Tuple
from xml.sax.saxutils import escape
import tomllib
import signal

STOP = False


# 信号处理，优雅退出
def _stop(*_):
    global STOP
    STOP = True


signal.signal(signal.SIGTERM, _stop)
signal.signal(signal.SIGINT, _stop)

CFG_PATH = Path("/config.toml")
OUT_DIR = Path("/out")

DEBUG = os.environ.get("DEBUG", "") == "1"
ONCE = os.environ.get("ONCE", "") == "1"

# 关键：-k 关闭 db lookup，确保 type 是 _http._tcp 这种真实类型
# 移除 -t，改为持续监听模式
AVAHI_BROWSE_CMD = ["avahi-browse", "-a", "-r", "-p", "-k"]

# 解析前缀
RESOLVED_PREFIX = "=;"
REMOVED_PREFIX = "-;"


def dprint(*a):
    if DEBUG:
        print(*a, flush=True)


def run(cmd: List[str]) -> str:
    return subprocess.check_output(cmd, text=True, stderr=subprocess.STDOUT, timeout=5)


# XML 1.0 不允许的大多数控制字符：0x00-0x1F（除 \t \n \r）以及 0x7F
_XML_ILLEGAL = re.compile(r"[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]")


def unescape_avahi_instance(s: str) -> str:
    r"""
    avahi-browse parsable 输出里会用 \DDD 表示一个字节（这里的 DDD 是“十进制”编码）
    典型例子：
      \032 => ASCII 32 => 空格
      \091 => ASCII 91 => '['
    有时你会看到 \\032（双反斜杠），这里都统一处理掉。

    注意：这里绝对不能按八进制解析，否则 \032 会变成 0x1A（^Z），导致 XML invalid token。
    """
    # 把双反斜杠压成单反斜杠
    s = s.replace("\\\\", "\\")

    # 替换 \DDD（十进制）转义
    def repl(m):
        try:
            v = int(m.group(1), 10)  # 十进制！
            if 0 <= v <= 255:
                return chr(v)
            return m.group(0)
        except Exception:
            return m.group(0)

    return re.sub(r"\\([0-9]{3})", repl, s)


def xml_safe_text(s: str) -> str:
    """
    确保写入 XML 的文本是合法的（去掉非法控制字符）。
    同时把多余空白压缩一下，避免名字变得怪。
    """
    s = unescape_avahi_instance(s)
    s = _XML_ILLEGAL.sub(" ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def sanitize_filename(s: str) -> str:
    s = unescape_avahi_instance(s)
    s = re.sub(r"[^A-Za-z0-9._-]+", "_", s)
    return s[:160] if len(s) > 160 else s


def load_config() -> Tuple[str, set, Dict[str, int], List[Dict[str, str]]]:
    with CFG_PATH.open("rb") as f:
        cfg = tomllib.load(f)

    prefix = str(cfg.get("filename_prefix", "gen-"))
    allow_types = set(cfg.get("allow_types", []))

    port_map_raw = cfg.get("port_map", {}) or {}
    port_map: Dict[str, int] = {str(k): int(v) for k, v in port_map_raw.items()}

    rewrites = cfg.get("txt_rewrite", []) or []
    rules: List[Dict[str, str]] = []
    for r in rewrites:
        if isinstance(r, dict) and "from" in r and "to" in r:
            rules.append({"from": str(r["from"]), "to": str(r["to"])})
    return prefix, allow_types, port_map, rules


def parse_line_parts(line: str) -> Dict[str, object] | None:
    """
    解析 avahi-browse -p 输出的行
    格式：Prefix;iface;proto;instance;type;domain;host;addr;port;txt...
    """
    parts = line.split(";")
    if len(parts) < 6:
        return None

    # 基础字段，Resolved 和 Removed 都有
    # Removed 行通常没有 host/addr/port/txt，只有前 6 个字段有效
    res: Dict[str, object] = {
        "prefix": parts[0],
        "iface": parts[1],
        "proto": parts[2],
        "instance_raw": parts[3],
        "instance": unescape_avahi_instance(parts[3]),
        "type": parts[4],
        "domain": parts[5],
    }

    # 只有 Resolved 行才有详细信息
    if res["prefix"] == RESOLVED_PREFIX and len(parts) >= 9:
        res["host"] = parts[6]
        res["addr"] = parts[7]
        try:
            res["port"] = int(parts[8])
        except ValueError:
            res["port"] = 0

        txt: List[str] = []
        for r in parts[9:]:
            r = r.strip()
            if r.startswith('"txt=') and r.endswith('"'):
                txt.append(r[5:-1])
            elif r.startswith("txt="):
                txt.append(r[4:].strip('"'))
        res["txt"] = txt

    return res


def rewrite_txt(txt: List[str], rules: List[Dict[str, str]]) -> List[str]:
    out = []
    for t in txt:
        t2 = t
        for r in rules:
            t2 = t2.replace(r["from"], r["to"])
        out.append(t2)
    return out


def make_service_xml(
    instance_name: str, stype: str, host_port: int, txt: List[str]
) -> str:
    # 写 XML 前做净化，避免 invalid token
    safe_name = xml_safe_text(instance_name)

    lines = [
        "<?xml version=\"1.0\" standalone='no'?><!--*-nxml-*-->",
        '<!DOCTYPE service-group SYSTEM "avahi-service.dtd">',
        "<service-group>",
        f'  <name replace-wildcards="yes">{escape(safe_name)}</name>',
        "  <service>",
        f"    <type>{escape(stype)}</type>",
        f"    <port>{host_port}</port>",
    ]
    for t in txt:
        safe_txt = xml_safe_text(t)
        if safe_txt:
            lines.append(f"    <txt-record>{escape(safe_txt)}</txt-record>")
    lines += ["  </service>", "</service-group>", ""]
    return "\n".join(lines)


def atomic_write(path: Path, content: str) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(content, encoding="utf-8")
    tmp.replace(path)


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    prefix, allow_types, port_map, rewrite_rules = load_config()
    dprint(
        "Loaded config:",
        "prefix",
        prefix,
        "allow_types",
        sorted(allow_types),
        "port_map_n",
        len(port_map),
        "rewrite_n",
        len(rewrite_rules),
    )

    # 启动时先清理旧文件
    for f in OUT_DIR.glob(f"{prefix}*.service"):
        f.unlink(missing_ok=True)
        dprint("startup cleanup:", f.name)

    # 内存索引：(iface, proto, instance, type) -> filename
    # 用于处理删除事件
    active_services: Dict[Tuple[str, str, str, str], str] = {}

    while not STOP:
        try:
            # 启动 avahi-browse 进程，持续监听
            dprint("Starting avahi-browse...")
            process = subprocess.Popen(
                AVAHI_BROWSE_CMD,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                bufsize=1,  # 行缓冲
            )

            if process.stdout is None:
                dprint("Failed to open stdout")
                time.sleep(5)
                continue

            for line in process.stdout:
                if STOP:
                    break

                line = line.strip()
                if not line:
                    continue

                # 只关心 =; 和 -;
                if not (
                    line.startswith(RESOLVED_PREFIX) or line.startswith(REMOVED_PREFIX)
                ):
                    continue

                info = parse_line_parts(line)
                if not info:
                    continue

                # 唯一标识符 key
                # 注意：avahi-browse 输出的 instance 包含转义字符，我们用原始字符串做 key 更稳妥
                # 或者统一用 unescaped 后的。这里用 info['instance'] (已 unescape)
                # key = (iface, proto, instance, type)
                srv_key = (
                    str(info["iface"]),
                    str(info["proto"]),
                    str(info["instance"]),
                    str(info["type"]),
                )

                if info["prefix"] == RESOLVED_PREFIX:
                    # === 新增/更新服务 ===
                    stype = str(info["type"])
                    addr = str(info["addr"])
                    port = int(info["port"])  # type: ignore

                    # 过滤逻辑
                    if allow_types and stype not in allow_types:
                        # dprint("skip type:", stype)
                        continue

                    # 端口映射检查
                    map_key = f"{addr}:{port}"
                    host_port = port_map.get(map_key)
                    if host_port is None:
                        # dprint("skip unmapped:", map_key)
                        continue

                    # 生成文件名
                    instance = str(info["instance"])
                    txt = rewrite_txt(info["txt"], rewrite_rules)  # type: ignore

                    fname = (
                        f"{prefix}"
                        f"{sanitize_filename(instance)}-"
                        f"{sanitize_filename(stype)}-"
                        f"{sanitize_filename(addr)}-"
                        f"{port}.service"
                    )

                    # 写入文件
                    xml = make_service_xml(instance, stype, host_port, txt)
                    atomic_write(OUT_DIR / fname, xml)

                    # 更新索引
                    active_services[srv_key] = fname
                    dprint("Resolved:", fname, "->", host_port)

                elif info["prefix"] == REMOVED_PREFIX:
                    # === 删除服务 ===
                    if srv_key in active_services:
                        fname = active_services[srv_key]
                        (OUT_DIR / fname).unlink(missing_ok=True)
                        del active_services[srv_key]
                        dprint("Removed:", fname)
                    else:
                        # dprint("Ignore unknown removal:", srv_key)
                        pass

            process.terminate()
            process.wait()

        except Exception:
            import traceback

            traceback.print_exc()
            time.sleep(5)

        if ONCE:
            break


if __name__ == "__main__":
    main()
