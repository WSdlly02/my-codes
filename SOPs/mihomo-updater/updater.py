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
SERVICE_NAME = "mihomo.service"

# Resolver 地址
RESOLVER_URL = "http://127.0.0.1:8088/config"

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


def download_data(url: str, dst: Path, timeout=30) -> bool:
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp_path = dst.with_name(dst.name + ".tmp")
    try:
        headers = {"User-Agent": "Clash/Meta"}
        req = urllib.request.Request(url, headers=headers)
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            data = resp.read()
            if len(data) < 50:
                return False
        tmp_path.write_bytes(data)
        tmp_path.replace(dst)
        print(f"✅ 下载成功: {dst.name}")
        return True
    except Exception as e:
        print(f"❌ 下载失败 {dst.name}: {e}")
        return False


def fetch_config(url: str, timeout=30):
    try:
        headers = {"User-Agent": "Clash/Meta"}
        req = urllib.request.Request(url, headers=headers)
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            data = resp.read()
            return yaml.safe_load(data)
    except Exception as e:
        print(f"❌ 获取配置失败: {e}")
        return None


def load_yaml(path: Path):
    if not path.exists():
        return {}
    with path.open("r", encoding="utf-8") as f:
        return yaml.safe_load(f) or {}


def save_yaml(data, path: Path) -> None:
    with path.open("w", encoding="utf-8") as f:
        yaml.safe_dump(
            data, f, allow_unicode=True, sort_keys=False, default_flow_style=False
        )


def restart_service():
    cmd = ["systemctl", "restart", SERVICE_NAME]
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
    remote_data = fetch_config(RESOLVER_URL)

    if not remote_data:
        print("❌ 无法获取远程配置，终止更新")
        return

    # 2. 读取本地配置
    local_data = load_yaml(LOCAL_CONFIG)

    # 3. 合并数据
    merged_data = dict(local_data)

    # 更新 proxies, proxy-groups, rules
    if "proxies" in remote_data:
        merged_data["proxies"] = remote_data["proxies"]
    else:
        print("❌ 远程配置缺少 proxies，终止更新")
        raise SystemExit(1)
    if "proxy-groups" in remote_data:
        merged_data["proxy-groups"] = remote_data["proxy-groups"]
    else:
        print("❌ 远程配置缺少 proxy-groups，终止更新")
        raise SystemExit(1)
    if "rules" in remote_data:
        merged_data["rules"] = remote_data["rules"]
    else:
        print("❌ 远程配置缺少 rules，终止更新")
        raise SystemExit(1)

    print(f"[merge] 配置合并完成")

    # 4. 保存备份
    if LOCAL_CONFIG.exists():
        ts = datetime.now().strftime("%Y%m%d%H%M")
        backup_path = MIHOMO_DIR / "backup" / f"config.bak.{ts}"
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        LOCAL_CONFIG.rename(backup_path)

    # 5. 保存新配置
    save_yaml(merged_data, LOCAL_CONFIG)
    print("✅ 配置文件已生成")

    # 6. 数据库更新
    print("[db] 检查数据库更新...")
    for key, cfg in DB_SOURCES.items():
        dst = MIHOMO_DIR / cfg["filename"]
        try:
            download_data(cfg["url"], dst)
        except:
            pass

    # 7. 重启服务
    try:
        print("[service] 重启 Mihomo 服务...")
        restart_service()
    except Exception as e:
        print(f"⚠️ 服务重启失败: {e}")

    print("=== 完成 ===")


if __name__ == "__main__":
    main()
