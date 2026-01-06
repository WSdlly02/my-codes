#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from dotenv import load_dotenv
from pathlib import Path
import os
import http.server
import socketserver
import urllib.request
import urllib.parse
import urllib.error
import yaml
import signal
import sys

# ==========================================
#              1. 配置区域
# ==========================================

load_dotenv()


def get_env_variable(var_name, default=None) -> str:
    value = os.getenv(var_name, default)
    if value is None:
        raise EnvironmentError(f"必需的环境变量未设置: {var_name}")
    return value


try:
    # 基础配置
    AIRPORT_URL = get_env_variable("AIRPORT_URL")
    ORIGIN_CONFIG_PATH = Path(get_env_variable("ORIGIN_CONFIG_PATH"))
    SUBCONVERTER_HOST = get_env_variable("SUBCONVERTER_HOST", "http://127.0.0.1:25500")
    PORT = int(get_env_variable("RESOLVER_PORT", "8088"))

    # VPS 配置
    JP_BYTEVIRT_VPS_NAME = "🇯🇵 日本 ByteVirt VPS"
    JP_BYTEVIRT_VPS_IP = get_env_variable("JP_BYTEVIRT_VPS_IP")
    JP_BYTEVIRT_VPS_UUID = get_env_variable("JP_BYTEVIRT_VPS_UUID")
    JP_BYTEVIRT_VPS_PORT = int(get_env_variable("JP_BYTEVIRT_VPS_PORT"))
    JP_BYTEVIRT_VPS_PUBKEY = get_env_variable("JP_BYTEVIRT_VPS_PUBKEY")
    JP_BYTEVIRT_VPS_DOMAIN = get_env_variable("JP_BYTEVIRT_VPS_DOMAIN")
except (EnvironmentError, ValueError) as e:
    print(f"配置文件错误: {e}")
    raise SystemExit(1)

# 规则模板
RULES_URL = "https://raw.githubusercontent.com/ACL4SSR/ACL4SSR/master/Clash/config/ACL4SSR_Online_Full.ini"

# 【自定义代理区】直接写 Clash Meta 格式的字典
CUSTOM_PROXY = [
    {
        "name": JP_BYTEVIRT_VPS_NAME,
        "type": "vless",
        "uuid": JP_BYTEVIRT_VPS_UUID,
        "server": JP_BYTEVIRT_VPS_IP,
        "port": JP_BYTEVIRT_VPS_PORT,
        "flow": "xtls-rprx-vision",
        "udp": True,
        "tls": True,
        "servername": "www.microsoft.com",
        "client-fingerprint": "chrome",
        "reality-opts": {
            "public-key": JP_BYTEVIRT_VPS_PUBKEY,
            "short-id": "",
        },
    },
]

# 【自定义规则区】(优先级最高)
CUSTOM_RULES = [
    "IP-CIDR,100.64.0.0/10,DIRECT,no-resolve",
    "DOMAIN-SUFFIX,tailscale.com,DIRECT",
    f"IP-CIDR,{JP_BYTEVIRT_VPS_IP}/32,DIRECT,no-resolve",
    f"DOMAIN,{JP_BYTEVIRT_VPS_DOMAIN},DIRECT",
]

# 【智能分组逻辑】
AUTO_GROUP_MAP = {
    "日本": [JP_BYTEVIRT_VPS_NAME],
    "自动": [JP_BYTEVIRT_VPS_NAME],
    "手动": [JP_BYTEVIRT_VPS_NAME],
}


def build_subconverter_url() -> str:
    """只生成机场的订阅转换链接"""
    args = [
        "target=clash",
        "url=" + urllib.parse.quote(AIRPORT_URL),  # 只有机场
        "config=" + urllib.parse.quote(RULES_URL),
        "insert=true",
        "emoji=true",
        "list=false",
        "tfo=true",
        "scv=false",
        "fdn=true",
        "expand=true",
        "sort=false",
        "udp=true",
        "new_name=true",
    ]
    return f"{SUBCONVERTER_HOST}/sub?{'&'.join(args)}"


def fetch_url(url, timeout=30):
    headers = {"User-Agent": "Clash/Meta"}
    req = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            if resp.status != 200:
                raise Exception(f"HTTP 错误: {resp.status}")
            return resp.read()
    except urllib.error.URLError as e:
        raise Exception(f"网络连接失败: {e.reason}")


def generate_config() -> dict:
    sub_url = build_subconverter_url()
    print(f"正在从 Subconverter 获取订阅: {sub_url}")
    data = fetch_url(sub_url)
    if not data or len(data) < 100:
        raise Exception("获取到的订阅数据过短或为空，可能 Subconverter 返回了错误信息")

    remote_data = yaml.safe_load(data)
    if not isinstance(remote_data, dict):
        raise Exception("解析订阅数据失败: 返回的内容不是有效的 YAML 字典")

    proxies = remote_data.get("proxies", [])
    proxy_groups = remote_data.get("proxy-groups", [])

    # 1. 注入自定义代理
    existing_names = {p["name"] for p in proxies if "name" in p}
    for my_node in CUSTOM_PROXY:
        if my_node["name"] not in existing_names:
            proxies.append(my_node)
            print(f"[inject] 已注入节点: {my_node['name']}")
        else:
            for i, p in enumerate(proxies):
                if p.get("name") == my_node["name"]:
                    proxies[i] = my_node
                    print(f"[inject] 已更新现有节点: {my_node['name']}")

    # 2. 注入到对应策略组
    for group in proxy_groups:
        group_name = group.get("name", "")
        for key_word, node_names in AUTO_GROUP_MAP.items():
            if key_word in group_name:
                for node_name in node_names:
                    if "proxies" not in group:
                        group["proxies"] = []
                    if node_name not in group["proxies"]:
                        group["proxies"].insert(0, node_name)

    # 3. 合并规则
    remote_data["rules"] = CUSTOM_RULES + remote_data.get("rules", [])
    remote_data["proxies"] = proxies
    remote_data["proxy-groups"] = proxy_groups

    return remote_data


def load_yaml(path: Path) -> dict:
    if not path.exists():
        raise FileNotFoundError(f"找不到原始配置文件: {path}")
    if not path.is_file():
        raise IsADirectoryError(f"路径不是文件: {path}")
    with path.open("r", encoding="utf-8") as f:
        data = yaml.safe_load(f)
        if data is None:
            return {}
        if not isinstance(data, dict):
            raise ValueError(
                f"原始配置文件格式错误，期望字典，实际得到: {type(data).__name__}"
            )
        return data


class ConfigHandler(http.server.BaseHTTPRequestHandler):
    def send_error_response(self, code, message):
        self.send_response(code)
        self.send_header("Content-type", "text/plain; charset=utf-8")
        self.end_headers()
        self.wfile.write(message.encode("utf-8"))

    def do_GET(self):
        try:
            match self.path:
                case "/health":
                    self.send_response(200)
                    self.end_headers()
                    self.wfile.write(b"OK")
                case "/config/minimal":
                    # generate_config 若失败将直接抛出异常，进入最下方的 except
                    config = generate_config()
                    response = yaml.safe_dump(
                        config,
                        allow_unicode=True,
                        sort_keys=False,
                        default_flow_style=False,
                    )
                    self.send_response(200)
                    self.send_header(
                        "Content-type", "application/x-yaml; charset=utf-8"
                    )
                    self.end_headers()
                    self.wfile.write(response.encode("utf-8"))

                case "/config/full":
                    config = generate_config()
                    origin_data = load_yaml(ORIGIN_CONFIG_PATH)

                    # 合并关键部分
                    origin_data["proxies"] = config.get("proxies", [])
                    origin_data["proxy-groups"] = config.get("proxy-groups", [])
                    origin_data["rules"] = config.get("rules", [])

                    response = yaml.safe_dump(
                        origin_data,
                        allow_unicode=True,
                        sort_keys=False,
                        default_flow_style=False,
                    )
                    self.send_response(200)
                    self.send_header(
                        "Content-type", "application/x-yaml; charset=utf-8"
                    )
                    self.end_headers()
                    self.wfile.write(response.encode("utf-8"))
                case _:
                    self.send_response(404)
                    self.end_headers()
                    self.wfile.write(b"Not Found")
        except Exception as e:
            # 统一捕获所有层级的异常并打印日志
            print(f"处理接口 {self.path} 时发生错误: {e}")
            self.send_error_response(500, f"服务内部错误: {e}")


def run_server():
    # Allow reuse address to avoid "Address already in use" errors during restarts
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("", PORT), ConfigHandler) as httpd:
        # 注册信号处理，确保 SIGTERM 能触发 SystemExit
        # SIGINT (Ctrl+C) 默认会抛出 KeyboardInterrupt
        signal.signal(signal.SIGTERM, lambda signum, frame: sys.exit(0))

        print(f"Serving at port {PORT}")
        try:
            httpd.serve_forever()
        except (KeyboardInterrupt, SystemExit):
            print("\n正在停止服务器...")
        finally:
            print("服务器已关闭。")


if __name__ == "__main__":
    run_server()
