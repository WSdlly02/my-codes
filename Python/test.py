from flask import Flask, render_template_string, Response, jsonify, request
import psutil
import subprocess
import os
from picamera2 import Picamera2
import time

app = Flask(__name__)

# 初始化摄像头（仅适用于树莓派）
camera = Picamera2()
camera_config = camera.create_video_configuration(main={"size": (640, 480)})
camera.configure(camera_config)

# 报警状态
alarm_active = False


def get_system_info():
    """获取系统信息"""
    mem = psutil.virtual_memory()
    disk = psutil.disk_usage("/")
    cpu_temp = None
    try:
        with open("/sys/class/thermal/thermal_zone0/temp", "r") as f:
            cpu_temp = round(int(f.read()) / 1000, 1)
    except:
        pass

    return {
        "cpu_usage": psutil.cpu_percent(),
        "mem_usage": mem.percent,
        "disk_usage": disk.percent,
        "cpu_temp": cpu_temp,
    }


def generate_frames():
    """生成视频流"""
    camera.start()
    try:
        while True:
            frame = camera.capture_array()
            ret, jpeg = cv2.imencode(".jpg", frame)
            yield (
                b"--frame\r\n"
                b"Content-Type: image/jpeg\r\n\r\n" + jpeg.tobytes() + b"\r\n\r\n"
            )
            time.sleep(0.1)
    finally:
        camera.stop()


@app.route("/")
def index():
    """主页面"""
    return render_template_string(
        """
        <!DOCTYPE html>
        <html>
        <head>
            <title>树莓派监控系统</title>
            <style>
                .container { display: grid; grid-template-columns: 2fr 1fr; gap: 20px; }
                .video-box { background: #333; padding: 10px; border-radius: 5px; }
                .info-box { background: #f5f5f5; padding: 20px; border-radius: 5px; }
                .controls { margin-top: 20px; display: flex; gap: 10px; }
                button { padding: 10px 20px; cursor: pointer; }
                #alarmBtn.active { background: red; color: white; }
            </style>
        </head>
        <body>
            <div class="container">
                <div class="video-box">
                    <h2>实时监控</h2>
                    < img src="/video_feed" style="max-width: 100%;">
                </div>
                
                <div class="info-box">
                    <h2>系统信息</h2>
                    <div id="systemInfo"></div>
                    
                    <div class="controls">
                        <button onclick="reboot()">重启系统</button>
                        <button id="alarmBtn" onclick="toggleAlarm()">触发警报</button>
                    </div>
                </div>
            </div>

            <script>
                // 更新系统信息
                function updateSystemInfo() {
                    fetch('/system_info')
                        .then(r => r.json())
                        .then(data => {
                            document.getElementById('systemInfo').innerHTML = `
                                CPU使用率: ${data.cpu_usage}%<br>
                                内存使用率: ${data.mem_usage}%<br>
                                磁盘使用率: ${data.disk_usage}%<br>
                                CPU温度: ${data.cpu_temp || 'N/A'}℃
                            `;
                        });
                }
                
                // 重启系统
                function reboot() {
                    if(confirm('确定要重启系统吗？')) {
                        fetch('/reboot', { method: 'POST' })
                            .then(r => alert('重启命令已发送'));
                    }
                }
                
                // 警报控制
                let alarm = new Audio('/static/alarm.mp3');
                function toggleAlarm() {
                    const btn = document.getElementById('alarmBtn');
                    if(alarm_active) {
                        alarm.pause();
                        btn.classList.remove('active');
                    } else {
                        alarm.loop = true;
                        alarm.play();
                        btn.classList.add('active');
                    }
                    alarm_active = !alarm_active;
                }
                
                // 每3秒更新信息
                setInterval(updateSystemInfo, 3000);
                updateSystemInfo();
            </script>
        </body>
        </html>
    """
    )


@app.route("/video_feed")
def video_feed():
    """视频流路由"""
    return Response(
        generate_frames(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


@app.route("/system_info")
def system_info():
    """系统信息API"""
    return jsonify(get_system_info())


@app.route("/reboot", methods=["POST"])
def reboot():
    """重启系统"""
    try:
        subprocess.run(["sudo", "reboot"], check=True)
        return jsonify({"status": "success"})
    except Exception as e:
        return jsonify({"status": "error", "message": str(e)}), 500


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000, threaded=True)
