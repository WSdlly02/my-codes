#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess
import urllib.request
from pathlib import Path
from datetime import datetime
import yaml

# ==========================================
#              1. 配置区域
# ==========================================

MIHOMO_DIR = Path("/home/wsdlly02/.config/mihomo")
LOCAL_CONFIG = MIHOMO_DIR / "config.yaml"
BACKUP_CONFIG = Path("/home/wsdlly02/Disks/Files/mihomo-config.yaml")
SERVICE_NAME = "mihomo.service"

# Resolver 地址
RESOLVER_URL = "http://127.0.0.1:8088/config/full"

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

# ==========================================
#              2. 核心逻辑
# ==========================================


def download_data(url: str, dst: Path, timeout=30) -> None:
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp_path = dst.with_name(dst.name + ".tmp")
    headers = {"User-Agent": "Clash/Meta"}
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        data = resp.read()
        if len(data) < 100:
            raise ValueError(f"下载的数据长度过短: {len(data)}")
    tmp_path.write_bytes(data)
    tmp_path.replace(dst)
    print(f"✅ 下载成功: {dst.name}")


def fetch_config(url: str, timeout=30):
    headers = {"User-Agent": "Clash/Meta"}
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        if resp.status != 200:
            raise Exception(f"HTTP 错误: {resp.status}")
        data = resp.read()
        return yaml.safe_load(data)


def save_yaml(data, path: Path) -> None:
    with path.open("w", encoding="utf-8") as f:
        yaml.safe_dump(
            data, f, allow_unicode=True, sort_keys=False, default_flow_style=False
        )


def restart_service() -> None:
    cmd = ["sudo", "systemctl", "restart", SERVICE_NAME]
    print(f"[service] 重启服务: {' '.join(cmd)}")
    subprocess.run(cmd, check=True)


# ==========================================
#              3. 主程序
# ==========================================


def main():
    print(f"=== Mihomo Injection Update: {datetime.now()} ===")

    MIHOMO_DIR.mkdir(parents=True, exist_ok=True)

    # 1. 从 Resolver 获取配置
    print(f"Fetching config from {RESOLVER_URL}...")
    try:
        remote_data = fetch_config(RESOLVER_URL)
        if not remote_data:
            raise Exception("获取到的配置为空")
        if not isinstance(remote_data, dict):
            raise Exception("返回的内容不是有效的 YAML 字典")
    except Exception as e:
        print(f"❌ 获取远程配置失败: {e}")
        raise SystemExit(1)
    print("✅ 远程完整配置获取成功")

    # 2. 保存备份
    if LOCAL_CONFIG.exists():
        ts = datetime.now().strftime("%Y%m%d-%H%M%S")
        backup_path = MIHOMO_DIR / "backup" / f"config.yaml.bak.{ts}"
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        LOCAL_CONFIG.rename(backup_path)

    # 3. 保存新配置
    try:
        save_yaml(remote_data, LOCAL_CONFIG)
        save_yaml(remote_data, BACKUP_CONFIG)
    except Exception as e:
        print(f"❌ 配置保存失败: {e}")
        raise SystemExit(1)
    print("✅ 配置文件、备份保存完成")

    # 4. 数据库更新
    print("[db] 检查数据库更新...")
    for key, cfg in DB_SOURCES.items():
        dst = MIHOMO_DIR / cfg["filename"]
        try:
            download_data(cfg["url"], dst)
        except Exception as e:
            print(f"⚠️ 数据库 {key} 更新失败: {e}，跳过")
            pass

    # 5. 重启服务
    try:
        print("[service] 重启 Mihomo 服务...")
        restart_service()
    except Exception as e:
        print(f"⚠️ 服务重启失败: {e}")
        raise SystemExit(1)

    print("=== 完成 ===")


if __name__ == "__main__":
    main()
