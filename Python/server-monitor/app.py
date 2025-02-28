from flask import Flask, render_template, jsonify, request
import psutil
import os
import subprocess
import logging

app = Flask(__name__)

API_TOKEN = "reboot-password"


def get_server_info():
    """获取服务器基本信息"""
    # CPU 信息
    cpu_percent = psutil.cpu_percent(interval=1)
    cpu_cores = psutil.cpu_count(logical=False)

    # 内存信息
    mem = psutil.virtual_memory()

    # 磁盘信息
    disk = psutil.disk_usage("/")

    # 网络信息
    net_io = psutil.net_io_counters()

    return {
        "cpu": {"percent": cpu_percent, "cores": cpu_cores},
        "memory": {
            "total": round(mem.total / (1024**3)),
            "used": round(mem.used / (1024**3)),
            "percent": mem.percent,
        },
        "disk": {
            "total": round(disk.total / (1024**3)),
            "used": round(disk.used / (1024**3)),
            "percent": disk.percent,
        },
        "network": {
            "sent": round(net_io.bytes_sent / (1024**2)),
            "recv": round(net_io.bytes_recv / (1024**2)),
        },
    }


@app.route("/")
def index():
    return render_template("index.html")


@app.route("/api/server-info")
def api_server_info():
    return jsonify(get_server_info())


@app.route("/api/reboot", methods=["POST"])
def api_reboot():
    """安全重启端点（需要令牌验证）"""
    # 验证令牌
    token = request.headers.get("X-API-TOKEN")
    if token != API_TOKEN:
        return jsonify({"status": "error", "message": "Unauthorized"}), 403

    try:
        # 执行重启命令（根据系统调整）
        subprocess.run(["sudo", "shutdown", "-r", "now"], check=True)
        return jsonify({"status": "success"})
    except Exception as e:
        logging.error(f"Reboot failed: {str(e)}")
        return jsonify({"status": "error", "message": str(e)}), 500


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8080, debug=True)
