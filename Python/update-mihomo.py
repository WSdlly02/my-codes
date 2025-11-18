#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
mihomo 配置更新脚本：
  1. 下载远程订阅配置
  2. 与本地 config.yaml 按规则合并（只更新指定字段）
  3. 下载/更新 geoip / geosite / mmdb / asn 数据文件
  4. 重启 mihomo

依赖：
  - Python 3
  - PyYAML:  python -m pip install pyyaml

建议：
  - 以用户 wsdlly02 身份运行（配置在 /home/wsdlly02/ 下）
  - mihomo 作为 user systemd 服务运行（systemctl --user）
"""

import subprocess
import tempfile
import urllib.request
from pathlib import Path
from datetime import datetime

import yaml


# ========= 需要根据实际情况调整的配置 =========

# 订阅 URL（你给的那条）
REMOTE_URL = "https://api.wcc.best/sub?target=clash&url=https%3A%2F%2Fsep.bbdmfetch.com%2Fapi%2Fv1%2Fclient%2Fsubscribe%3Ftoken%3D27a76c57a364bb7fe6952a862ea592a9&insert=true&config=https%3A%2F%2Fraw.githubusercontent.com%2FACL4SSR%2FACL4SSR%2Fmaster%2FClash%2Fconfig%2FACL4SSR_Online_Full.ini&emoji=true&list=false&tfo=false&scv=false&fdn=true&expand=true&sort=false&udp=true&new_name=true"

# mihomo 配置目录 / 文件
MIHOMO_DIR = Path("/home/wsdlly02/.config/mihomo")
LOCAL_CONFIG = MIHOMO_DIR / "config.yaml"

# mihomo 的 systemd 服务名
SERVICE_NAME = "mihomo.service"

# 从远程配置中直接覆盖的键（常见：proxies / proxy-groups / rules）
KEYS_FROM_REMOTE = ["proxies", "proxy-groups", "rules"]

# 是否在写回之前备份本地配置
ENABLE_BACKUP = True
BACKUP_DIR = MIHOMO_DIR / "backup"

# 数据库文件下载配置：key -> {url, filename}
DB_SOURCES = {
    "geoip": {
        "url": "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geoip-lite.dat",
        "filename": "geoip-lite.dat",
    },
    "geosite": {
        "url": "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geosite.dat",
        "filename": "geosite.dat",
    },
    "mmdb": {
        "url": "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/geoip.metadb",
        "filename": "geoip.metadb",
    },
    "asn": {
        "url": "https://github.com/MetaCubeX/meta-rules-dat/releases/download/latest/GeoLite2-ASN.mmdb",
        "filename": "GeoLite2-ASN.mmdb",
    },
}


# ========= 工具函数 =========


def download_to_path(url: str, dst: Path) -> None:
    """下载 URL 到指定路径，使用临时文件再原子替换。"""
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp_path = dst.with_name(dst.name + ".tmp")

    print(f"[mihomo-update] downloading: {url} -> {dst}")
    req = urllib.request.Request(
        url,
        headers={
            "User-Agent": "curl/8.0.0",
            "Accept": "*/*",
        },
    )
    with urllib.request.urlopen(req) as resp:
        data = resp.read()

    tmp_path.write_bytes(data)
    tmp_path.replace(dst)
    print(f"[mihomo-update] downloaded -> {dst} ({len(data)} bytes)")


def load_yaml(path: Path):
    if not path.exists():
        return {}
    with path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f) or {}


def save_yaml(data, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
        yaml.safe_dump(
            data,
            f,
            allow_unicode=True,
            sort_keys=False,
            default_flow_style=False,
        )


def backup_local_config(config_path: Path) -> None:
    if not config_path.exists():
        return
    BACKUP_DIR.mkdir(parents=True, exist_ok=True)
    ts = datetime.now().strftime("%Y%m%d-%H%M%S")
    backup_file = BACKUP_DIR / f"{config_path.name}.bak.{ts}"
    backup_file.write_bytes(config_path.read_bytes())
    print(f"[mihomo-update] backup created -> {backup_file}")


def merge_configs(old_cfg: dict, remote_cfg: dict) -> dict:
    """
    合并策略：
      - 以 old_cfg 为基础
      - KEYS_FROM_REMOTE：用远程覆盖
      - OPTIONAL_KEYS_FROM_REMOTE：本地没有时，从远程补上
      - 其他字段保持旧配置不变（比如端口、tun、dns、自定义规则等）
    """
    merged = dict(old_cfg)

    # 覆盖的键
    for key in KEYS_FROM_REMOTE:
        if key in remote_cfg:
            merged[key] = remote_cfg[key]
            print(f"[mihomo-update] updated key from remote: {key}")

    return merged


def restart_service(service_name: str) -> None:
    """重启 mihomo 服务."""
    cmd = ["sudo", "systemctl", "restart", service_name]

    print(f"[mihomo-update] restarting service: {' '.join(cmd)}")
    subprocess.run(cmd, check=True)
    print("[mihomo-update] service restarted.")


def update_databases(base_dir: Path) -> None:
    """下载 / 更新 geoip、geosite、mmdb、asn 等数据库文件。"""
    for key, cfg in DB_SOURCES.items():
        url = cfg["url"]
        filename = cfg["filename"]
        dst = base_dir / filename
        try:
            download_to_path(url, dst)
            print(f"[mihomo-update] updated db [{key}] -> {dst}")
        except Exception as e:
            print(
                f"[mihomo-update] WARNING: failed to update db [{key}] from {url}: {e}"
            )


# ========= 主流程 =========


def main():
    print("[mihomo-update] start")

    MIHOMO_DIR.mkdir(parents=True, exist_ok=True)

    # 1. 下载远程订阅配置到临时文件
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_remote = Path(tmpdir) / "remote.yaml"
        print(f"[mihomo-update] downloading remote config from REMOTE_URL")
        download_to_path(REMOTE_URL, tmp_remote)

        # 2. 如果本地没有配置，直接初始化
        # if not LOCAL_CONFIG.exists():
        #     print("[mihomo-update] local config not found, initializing with remote...")
        #     LOCAL_CONFIG.write_bytes(tmp_remote.read_bytes())
        #     # 数据库也顺便更新
        #     update_databases(MIHOMO_DIR)
        #     restart_service(SERVICE_NAME)
        #     print("[mihomo-update] done (initialized).")
        #     return

        # 3. 读取本地与远程 YAML
        old_cfg = load_yaml(LOCAL_CONFIG)
        remote_cfg = load_yaml(tmp_remote)

        # 4. 合并配置
        merged_cfg = merge_configs(old_cfg, remote_cfg)

        # 5. 备份 + 写回
        if ENABLE_BACKUP:
            backup_local_config(LOCAL_CONFIG)

        save_yaml(merged_cfg, LOCAL_CONFIG)
        print(f"[mihomo-update] merged config saved -> {LOCAL_CONFIG}")

    # 6. 更新数据库文件
    update_databases(MIHOMO_DIR)

    # 7. 重启 mihomo
    restart_service(SERVICE_NAME)

    print("[mihomo-update] done.")


if __name__ == "__main__":
    main()
