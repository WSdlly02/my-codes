#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from dotenv import load_dotenv
import os
import http.server
import socketserver
import urllib.request
import urllib.parse
import yaml

# ==========================================
#              1. 配置区域
# ==========================================

load_dotenv()


def get_env_variable(var_name) -> str:
    value = os.getenv(var_name)
    if not value:
        print(f"请设置环境变量 {var_name}")
        raise EnvironmentError(f"请设置环境变量 {var_name}")
    return value


"""
AIRPORT_URL: 机场订阅链接
JP_BYTEVIRT_VPS_UUID: 日本 ByteVirt VPS 的 UUID
JP_BYTEVIRT_VPS_IP: 日本 ByteVirt VPS 的 IP 地址
JP_BYTEVIRT_VPS_PORT: 日本 ByteVirt VPS 的端口
JP_BYTEVIRT_VPS_PUBKEY: 日本 ByteVirt VPS 的 Reality 公钥
JP_BYTEVIRT_VPS_DOMAIN: 日本 ByteVirt VPS 的域名
"""

# 机场订阅链接
AIRPORT_URL = get_env_variable("AIRPORT_URL")
# 规则模板
RULES_URL = "https://raw.githubusercontent.com/ACL4SSR/ACL4SSR/master/Clash/config/ACL4SSR_Online_Full.ini"
# 本地 Subconverter
SUBCONVERTER_HOST = "http://127.0.0.1:25500"

# 【自定义代理区】直接写 Clash Meta 格式的字典
# 脚本会直接把它"注入"到配置文件，绕过 Subconverter 的解析
CUSTOM_PROXY = [
    {
        "name": "🇯🇵 日本 ByteVirt VPS",
        "type": "vless",
        "uuid": get_env_variable("JP_BYTEVIRT_VPS_UUID"),
        "server": get_env_variable("JP_BYTEVIRT_VPS_IP"),
        "port": int(get_env_variable("JP_BYTEVIRT_VPS_PORT")),
        "flow": "xtls-rprx-vision",
        "udp": True,
        "tls": True,
        "servername": "yahoo.co.jp",
        "client-fingerprint": "chrome",
        "reality-opts": {
            "public-key": get_env_variable("JP_BYTEVIRT_VPS_PUBKEY"),
            "short-id": "",
        },
    },
]

# 【自定义规则区】(优先级最高)
CUSTOM_RULES = [
    "IP-CIDR,100.64.0.0/10,DIRECT,no-resolve",
    "DOMAIN-SUFFIX,tailscale.com,DIRECT",
    f"IP-CIDR,{get_env_variable('JP_BYTEVIRT_VPS_IP')}/32,DIRECT,no-resolve",
    f"DOMAIN,{get_env_variable('JP_BYTEVIRT_VPS_DOMAIN')},DIRECT",
]

# 【智能分组逻辑】
# 脚本会自动把 VPS 插入到包含以下关键词的策略组中
# 格式: "策略组关键词": ["节点名称关键词"] (这里填 VPS 名字的一部分即可)
AUTO_GROUP_MAP = {
    "日本": ["日本 ByteVirt VPS"],
    "自动": ["日本 ByteVirt VPS"],  # 对应 ♻️ 自动选择
    "手动": ["日本 ByteVirt VPS"],  # 对应 🚀 手动切换
}

PORT = 8088


def build_subconverter_url():
    """只生成机场的订阅转换链接"""
    args = [
        "target=clash",
        "url=" + urllib.parse.quote(AIRPORT_URL),  # 只有机场
        "config=" + urllib.parse.quote(RULES_URL),
        "insert=true",
        "emoji=true",
        "list=false",
        "tfo=false",
        "scv=false",
        "fdn=true",
        "sort=false",
        "udp=true",
        "new_name=true",
    ]
    return f"{SUBCONVERTER_HOST}/sub?{'&'.join(args)}"


def fetch_url(url, timeout=30):
    headers = {"User-Agent": "Clash/Meta"}
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def generate_config():
    sub_url = build_subconverter_url()
    try:
        print(f"Fetching from: {sub_url}")
        data = fetch_url(sub_url)
        if len(data) < 50:
            print("Data too short")
            return None

        remote_data = yaml.safe_load(data)
        if not remote_data:
            print("YAML load failed")
            return None

        proxies = remote_data.get("proxies", [])
        proxy_groups = remote_data.get("proxy-groups", [])

        # 3. 【核心注入】将自定义 VPS 加入 proxies 列表
        # 先去重，防止重复添加
        existing_names = set(p["name"] for p in proxies)
        for my_node in CUSTOM_PROXY:
            if my_node["name"] not in existing_names:
                proxies.append(my_node)  # 直接加入字典！无需转换
                print(f"[inject] 已注入节点: {my_node['name']}")
            else:
                # 如果同名，覆盖旧的
                for i, p in enumerate(proxies):
                    if p["name"] == my_node["name"]:
                        proxies[i] = my_node

        # 4. 【智能分组】遍历策略组，把 VPS 插进去
        for group in proxy_groups:
            group_name = group["name"]

            # 兜底：如果是手动选择组，必须插进去
            # 判断逻辑：根据 AUTO_GROUP_MAP 的关键词匹配
            for key_word, node_keywords in AUTO_GROUP_MAP.items():
                if key_word in group_name:
                    # 找到需要插入的节点
                    for my_node in CUSTOM_PROXY:
                        # 如果我的节点名包含定义的关键词 (如 "日本VPS")
                        if any(k in my_node["name"] for k in node_keywords):
                            # 插入到第一位 (unshift)
                            if "proxies" not in group:
                                group["proxies"] = []
                            if my_node["name"] not in group["proxies"]:
                                group["proxies"].insert(0, my_node["name"])

        # 5. 规则合并 (自定义在先)
        base_rules = remote_data.get("rules", [])
        remote_data["rules"] = CUSTOM_RULES + base_rules

        # 确保 proxies 和 proxy-groups 更新回 remote_data
        remote_data["proxies"] = proxies
        remote_data["proxy-groups"] = proxy_groups

        return remote_data

    except Exception as e:
        print(f"Error generating config: {e}")
        return None


class ConfigHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/config":
            config = generate_config()
            if config:
                response = yaml.safe_dump(
                    config,
                    allow_unicode=True,
                    sort_keys=False,
                    default_flow_style=False,
                )
                self.send_response(200)
                self.send_header("Content-type", "application/x-yaml; charset=utf-8")
                self.end_headers()
                self.wfile.write(response.encode("utf-8"))
            else:
                self.send_response(500)
                self.end_headers()
                self.wfile.write(b"Error generating config")
        else:
            self.send_response(404)
            self.end_headers()


def run_server():
    # Allow reuse address to avoid "Address already in use" errors during restarts
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("", PORT), ConfigHandler) as httpd:
        print(f"Serving at port {PORT}")
        httpd.serve_forever()


if __name__ == "__main__":
    run_server()
