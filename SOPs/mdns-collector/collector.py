#!/usr/bin/env python3
import re
import subprocess
import time
from pathlib import Path
from typing import Dict, List, Tuple
from xml.sax.saxutils import escape

# Python 3.11+ 标准库
import tomllib

CFG_PATH = Path("/config.toml")
OUT_DIR = Path("/out")

AVAHI_BROWSE_CMD = [
    "avahi-browse",
    "-a",
    "-r",
    "-t",
    "-p",
    "-k",
]  # all, resolve, terminate, parsable


def _run(cmd: List[str]) -> str:
    return subprocess.check_output(cmd, text=True)


def _sanitize_filename(s: str) -> str:
    # 生成文件名用：保守一点
    s = re.sub(r"[^A-Za-z0-9._-]+", "_", s)
    return s[:120] if len(s) > 120 else s


def load_config() -> Tuple[int, str, set, Dict[str, int], List[Dict[str, str]]]:
    cfg = tomllib.loads(CFG_PATH.read_text())

    interval = int(cfg.get("interval_seconds", 5))
    prefix = str(cfg.get("filename_prefix", "gen-"))
    allow_types = set(cfg.get("allow_types", []))

    port_map_raw = cfg.get("port_map", {})
    port_map: Dict[str, int] = {str(k): int(v) for k, v in port_map_raw.items()}

    rewrites = cfg.get("txt_rewrite", [])
    # rewrites: list of {from,to}
    rewrites_norm: List[Dict[str, str]] = []
    for r in rewrites:
        if isinstance(r, dict) and "from" in r and "to" in r:
            rewrites_norm.append({"from": str(r["from"]), "to": str(r["to"])})
    return interval, prefix, allow_types, port_map, rewrites_norm


def parse_avahi_browse(output: str) -> List[dict]:
    """
    解析 avahi-browse -p 输出的 resolved 行：
    =;iface;proto;instance;type;domain;host;addr;port;[...txt fields...]
    """
    services = []
    for line in output.splitlines():
        if not line.startswith("=;"):
            continue
        parts = line.split(";")
        # 最少 9 个字段（最后可能还有 txt）
        if len(parts) < 9:
            continue

        instance = parts[3]
        stype = parts[4]
        domain = parts[5]
        host = parts[6]
        addr = parts[7]
        try:
            port = int(parts[8])
        except ValueError:
            continue

        txt_records: List[str] = []
        for r in parts[9:]:
            r = r.strip()
            # 常见形态: "txt=path=/"
            if r.startswith('"txt=') and r.endswith('"'):
                txt_records.append(r[5:-1])
            # 兼容一些变体（很少见）
            elif r.startswith("txt="):
                txt_records.append(r[4:].strip('"'))

        services.append(
            {
                "instance": instance,
                "type": stype,
                "domain": domain,
                "host": host,
                "addr": addr,
                "port": port,
                "txt": txt_records,
            }
        )
    return services


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
    # 不写 <host-name>：默认就是宿主机（wsdlly02-pc.local），满足你的“对外统一入口”
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

    while True:
        try:
            raw = _run(AVAHI_BROWSE_CMD)
            services = parse_avahi_browse(raw)

            keep = set()

            for s in services:
                stype = s["type"]
                if allow_types and stype not in allow_types:
                    continue

                key = f'{s["addr"]}:{s["port"]}'
                host_port = port_map.get(key)
                if host_port is None:
                    # 没映射就不导出（避免广播“不可达服务”）
                    continue

                instance = s["instance"]
                txt = rewrite_txt(s["txt"], rewrite_rules)

                # 文件名尽量唯一：instance + type + addr + cport
                fname = (
                    f"{prefix}"
                    f"{_sanitize_filename(instance)}-"
                    f"{_sanitize_filename(stype)}-"
                    f"{_sanitize_filename(s['addr'])}-"
                    f"{s['port']}.service"
                )

                keep.add(fname)
                xml = make_service_xml(instance, stype, host_port, txt)
                atomic_write(OUT_DIR / fname, xml)

            # 清理 stale：删掉本轮没看到的 gen-*.service
            for f in OUT_DIR.glob(f"{prefix}*.service"):
                if f.name not in keep:
                    f.unlink(missing_ok=True)

        except Exception:
            # collector 不要崩；下轮继续
            pass

        time.sleep(interval)


if __name__ == "__main__":
    main()
