from flask import Flask, render_template, jsonify
import psutil
import time

app = Flask(__name__)


# 获取系统信息
def get_system_info():
    # 内存信息
    mem = psutil.virtual_memory()
    # 网络信息（获取初始流量）
    net_io = psutil.net_io_counters()
    # CPU 信息
    cpu_percent = psutil.cpu_percent(interval=1)

    return {
        "memory": {
            "total": round(mem.total / (1024**3)),  # 转换为 GB
            "used": round(mem.used / (1024**3)),
            "percent": mem.percent,
        },
        "network": {
            "sent": round(net_io.bytes_sent / (1024**2)),  # 转换为 MB
            "recv": round(net_io.bytes_recv / (1024**2)),
        },
        "cpu": cpu_percent,
        "timestamp": int(time.time()),  # 时间戳用于前端更新
    }


# 主页面路由
@app.route("/")
def index():
    return render_template("index.html")


# 数据接口路由
@app.route("/api/data")
def api_data():
    return jsonify(get_system_info())


if __name__ == "__main__":
    app.run(debug=True, host="0.0.0.0")
