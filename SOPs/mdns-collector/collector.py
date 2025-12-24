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
AVAHI_BROWSE_CMD = ["avahi-browse", "-a", "-r", "-t", "-p", "-k"]

# 只处理 resolved 行
RESOLVED_PREFIX = "=;"


def dprint(*a):
    if DEBUG:
        print(*a, flush=True)


def run(cmd: List[str]) -> str:
    return subprocess.check_output(cmd, text=True, stderr=subprocess.STDOUT, timeout=5)


def unescape_avahi_instance(s: str) -> str:
    """
    avahi-browse parsable 输出里会用 \032 表示空格。
    有时你会看到 \\032（双反斜杠），这里都统一处理掉。
    """
    # 先把双反斜杠压成单反斜杠
    s = s.replace("\\\\", "\\")

    # 替换 \DDD 八进制转义
    def repl(m):
        try:
            return chr(int(m.group(1), 8))
        except Exception:
            return m.group(0)

    return re.sub(r"\\([0-7]{3})", repl, s)


def sanitize_filename(s: str) -> str:
    s = unescape_avahi_instance(s)
    s = re.sub(r"[^A-Za-z0-9._-]+", "_", s)
    return s[:160] if len(s) > 160 else s


def load_config() -> Tuple[int, str, set, Dict[str, int], List[Dict[str, str]]]:
    with CFG_PATH.open("rb") as f:
        cfg = tomllib.load(f)

    interval = int(cfg.get("interval_seconds", 5))
    prefix = str(cfg.get("filename_prefix", "gen-"))
    allow_types = set(cfg.get("allow_types", []))

    port_map_raw = cfg.get("port_map", {}) or {}
    port_map: Dict[str, int] = {str(k): int(v) for k, v in port_map_raw.items()}

    rewrites = cfg.get("txt_rewrite", []) or []
    rules: List[Dict[str, str]] = []
    for r in rewrites:
        if isinstance(r, dict) and "from" in r and "to" in r:
            rules.append({"from": str(r["from"]), "to": str(r["to"])})
    return interval, prefix, allow_types, port_map, rules


def parse_resolved_lines(output: str) -> List[dict]:
    """
    解析 =; 行格式：
    =;iface;proto;instance;type;domain;host;addr;port;["txt=..."]
    你给的输出正是这种结构。
    """
    res = []
    for line in output.splitlines():
        if not line.startswith(RESOLVED_PREFIX):
            continue
        parts = line.split(";")
        if len(parts) < 9:
            continue

        iface = parts[1]
        proto = parts[2]
        instance_raw = parts[3]
        stype = parts[4]
        domain = parts[5]
        host = parts[6]
        addr = parts[7]
        try:
            port = int(parts[8])
        except ValueError:
            continue

        txt: List[str] = []
        for r in parts[9:]:
            r = r.strip()
            if r.startswith('"txt=') and r.endswith('"'):
                txt.append(r[5:-1])
            elif r.startswith("txt="):
                txt.append(r[4:].strip('"'))

        res.append(
            {
                "iface": iface,
                "proto": proto,
                "instance_raw": instance_raw,
                "instance": unescape_avahi_instance(instance_raw),
                "type": stype,
                "domain": domain,
                "host": host,
                "addr": addr,
                "port": port,
                "txt": txt,
            }
        )
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
    # 不写 host-name => 对外统一成宿主机的 .local
    lines = [
        "<?xml version=\"1.0\" standalone='no'?><!--*-nxml-*-->",
        '<!DOCTYPE service-group SYSTEM "avahi-service.dtd">',
        "<service-group>",
        f'  <name replace-wildcards="yes">{escape(instance_name)}</name>',
        "  <service>",
        f"    <type>{escape(stype)}</type>",
        f"    <port>{host_port}</port>",
    ]
    for t in txt:
        lines.append(f"    <txt-record>{escape(t)}</txt-record>")
    lines += ["  </service>", "</service-group>", ""]
    return "\n".join(lines)


def atomic_write(path: Path, content: str) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(content)
    tmp.replace(path)


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    interval, prefix, allow_types, port_map, rewrite_rules = load_config()
    dprint(
        "Loaded config:",
        "interval",
        interval,
        "prefix",
        prefix,
        "allow_types",
        sorted(allow_types),
        "port_map_n",
        len(port_map),
        "rewrite_n",
        len(rewrite_rules),
    )

    while not STOP:
        keep = set()
        try:
            out = run(AVAHI_BROWSE_CMD)
            services = parse_resolved_lines(out)
            dprint("resolved services:", len(services))

            for s in services:
                stype = s["type"]
                key = f'{s["addr"]}:{s["port"]}'

                if allow_types and stype not in allow_types:
                    dprint("skip type:", stype, "instance:", s["instance"], "key:", key)
                    continue

                host_port = port_map.get(key)
                if host_port is None:
                    dprint(
                        "skip unmapped:", stype, "instance:", s["instance"], "key:", key
                    )
                    continue

                instance = s["instance"]
                txt = rewrite_txt(s["txt"], rewrite_rules)

                fname = (
                    f"{prefix}"
                    f"{sanitize_filename(instance)}-"
                    f"{sanitize_filename(stype)}-"
                    f"{sanitize_filename(s['addr'])}-"
                    f"{s['port']}.service"
                )
                keep.add(fname)

                xml = make_service_xml(instance, stype, host_port, txt)
                atomic_write(OUT_DIR / fname, xml)
                dprint("emit:", fname, "->", host_port)

            # 清理 stale
            for f in OUT_DIR.glob(f"{prefix}*.service"):
                if f.name not in keep:
                    f.unlink(missing_ok=True)
                    dprint("remove stale:", f.name)

        except Exception as e:
            import traceback

            traceback.print_exc()

        if ONCE:
            break
        if STOP:
            break
        time.sleep(interval)


if __name__ == "__main__":
    main()
