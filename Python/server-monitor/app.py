from flask import Flask, render_template, jsonify
import psutil
import time

app = Flask(__name__)


def get_system_info():
    mem = psutil.virtual_memory()
    net_io = psutil.net_io_counters()
    cpu_percent = psutil.cpu_percent(interval=1, percpu=True)  # 获取每个核心的占用率

    return {
        "memory": {
            "total": round(mem.total / (1024**3)),
            "used": round(mem.used / (1024**3)),
            "percent": mem.percent,
        },
        "network": {
            "sent": round(net_io.bytes_sent / (1024**2)),
            "recv": round(net_io.bytes_recv / (1024**2)),
        },
        "cpu": sum(cpu_percent) / len(cpu_percent),  # 平均使用率
        "cpu_cores": cpu_percent,  # 每个核心的占用率
        "timestamp": int(time.time()),
    }


@app.route("/")
def index():
    return render_template("index.html")


@app.route("/api/data")
def api_data():
    return jsonify(get_system_info())


if __name__ == "__main__":
    app.run(debug=True, host="::")
