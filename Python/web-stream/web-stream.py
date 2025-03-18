from picamera2 import Picamera2
from flask import Flask, render_template_string, Response
import adafruit_mlx90640
import board
import cv2
import numpy as np
import psutil
import time
import threading

app = Flask(__name__)

# 初始化摄像头
i2c = board.I2C()
mlx = adafruit_mlx90640.MLX90640(i2c)
mlx.refresh_rate = adafruit_mlx90640.RefreshRate.REFRESH_8_HZ

# 全局变量
thermal_lock = threading.Lock()
frame = [0] * 768

# 颜色映射配置
COLORMAP = cv2.COLORMAP_JET  # 使用OpenCV的Jet色图
INTERPOLATION = cv2.INTER_CUBIC  # 插值算法
SCALE_FACTOR = 10  # 图像放大倍数 (32x24 -> 320x240)

# 摄像头配置
picam2 = Picamera2()
config = picam2.create_video_configuration(
    main={"size": (2592, 1944)},  # 原生分辨率
    lores={"size": (1296, 972)},  # 预览流
    display="main",
    encode="main",
    controls={"FrameRate": 30},
)
picam2.configure(config)
picam2.start()

frame_lock = threading.Lock()
latest_frame = None
sys_info = {"cpu": "0%", "memory": "0%", "temp": "N/A", "fps": "0"}


def adaptive_quality():
    """动态质量调整"""
    cpu = psutil.cpu_percent()
    net = psutil.net_io_counters().bytes_sent
    return 90 if cpu < 70 and net < 1e6 else 75


def capture_camera_frames():
    global latest_frame
    while True:
        with frame_lock:
            frame = picam2.capture_array("main")  # 获取高分辨率流
            frame = cv2.cvtColor(frame, cv2.COLOR_RGB2BGR)

            # 锐化处理
            kernel = np.array([[-1, -1, -1], [-1, 9, -1], [-1, -1, -1]])
            frame = cv2.filter2D(frame, -1, kernel)

            quality = adaptive_quality()
            _, jpeg = cv2.imencode(
                ".jpg", frame, [int(cv2.IMWRITE_JPEG_QUALITY), quality]
            )
            latest_frame = jpeg.tobytes()
        time.sleep(0.03)


def generate_camera():
    """生成 MJPEG 流"""
    while True:
        if latest_frame:
            yield (
                b"--frame\r\n"
                b"Content-Type: image/jpeg\r\n\r\n" + latest_frame + b"\r\n\r\n"
            )


def get_system_info():
    """获取系统信息"""
    while True:
        # CPU使用率
        cpu_percent = psutil.cpu_percent()

        # 内存使用
        mem = psutil.virtual_memory()

        # 温度
        temp = "N/A"
        with open("/sys/class/thermal/thermal_zone0/temp") as f:
            temp = f"{float(f.read()) / 1000:.1f}°C"

        with thermal_lock:
            sys_info.update(
                {
                    "cpu": f"{cpu_percent}%",
                    "memory": f"{mem.percent}%",
                    "temp": temp,
                    "fps": f"{np.random.randint(15, 30):02d}",  # 模拟FPS
                }
            )
        time.sleep(1)


def capture_thermal_data():
    """持续读取温度数据的线程"""
    global frame
    while True:
        try:
            with thermal_lock:
                mlx.getFrame(frame)
        except ValueError:
            continue  # 忽略偶发的I2C错误
        time.sleep(1 / 8)  # 匹配8Hz刷新率


def process_thermal_data():
    """处理温度数据生成彩色图像"""
    with thermal_lock:
        # 转换为numpy数组
        data = np.array(frame).reshape((24, 32)).astype(np.float32)

        # 归一化到0-255
        min_temp = 10
        max_temp = 60
        norm = ((data - min_temp) / (max_temp - min_temp) * 255).astype(np.uint8)

        # 应用颜色映射
        colored = cv2.applyColorMap(norm, COLORMAP)

        # 放大图像并优化显示
        scaled = cv2.resize(
            colored, (32 * SCALE_FACTOR, 24 * SCALE_FACTOR), interpolation=INTERPOLATION
        )

        # 添加温度刻度
        cv2.putText(
            scaled,
            f"{min_temp:.1f}C",
            (10, 30),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.6,
            (255, 255, 255),
            2,
        )
        cv2.putText(
            scaled,
            f"{max_temp:.1f}C",
            (scaled.shape[1] - 120, 30),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.6,
            (255, 255, 255),
            2,
        )
        if np.max(data) > 50:  # 高温报警阈值
            cv2.putText(
                scaled,
                "HIGH TEMP ALERT!",
                (scaled.shape[1] // 2 - 100, 50),
                cv2.FONT_HERSHEY_SIMPLEX,
                0.8,
                (0, 0, 255),
                2,
            )

        return scaled


def generate_thermal():
    """生成MJPEG视频流"""
    while True:
        frame = process_thermal_data()
        ret, jpeg = cv2.imencode(".jpg", frame)
        yield (
            b"--frame\r\n"
            b"Content-Type: image/jpeg\r\n\r\n" + jpeg.tobytes() + b"\r\n\r\n"
        )


@app.route("/")
def index():
    """主页面"""
    return render_template_string(
        """
    <!DOCTYPE html>
    <html>
    <head>
        <title>双画面监控系统</title>
        <style>
            body {
                margin: 0;
                padding: 20px;
                background: #1a1a1a;
                color: white;
                font-family: Arial, sans-serif;
            }
            .container {
                display: grid;
                grid-template-columns: 1fr 300px 1fr;
                gap: 20px;
                height: 90vh;
            }
            .video-box {
                background: #2a2a2a;
                border-radius: 8px;
                padding: 10px;
                box-shadow: 0 0 10px rgba(0,0,0,0.5);
            }
            .stats {
                display: flex;
                flex-direction: column;
                justify-content: center;
            }
            .stat-item {
                margin: 15px 0;
                padding: 15px;
                background: #333;
                border-radius: 6px;
            }
            h2 {
                color: #4CAF50;
                margin: 0 0 10px 0;
            }
            img {
                width: 100%;
                height: 100%;
                object-fit: contain;
            }
        </style>
    </head>
    <body>
        <h1>双画面监控系统</h1>
        <div class="container">
            <div class="video-box">
                <h2>可见光画面</h2>
                <img src="/video_feed">
            </div>
            
            <div class="stats">
                <div class="stat-item">
                    <h3>CPU 使用率</h3>
                    <div id="cpu">Loading...</div>
                </div>
                <div class="stat-item">
                    <h3>内存使用</h3>
                    <div id="memory">Loading...</div>
                </div>
                <div class="stat-item">
                    <h3>系统温度</h3>
                    <div id="temp">Loading...</div>
                </div>
                <div class="stat-item">
                    <h3>视频FPS</h3>
                    <div id="fps">Loading...</div>
                </div>
            </div>
            
            <div class="video-box">
                <h2>热成像画面</h2>
                <img src="/thermal_feed">
            </div>
        </div>

        <script>
            function updateSystemInfo() {
                fetch("/system_info")
                    .then(response => response.json())
                    .then(data => {
                        document.getElementById("cpu").innerHTML = data.cpu;
                        document.getElementById("memory").innerHTML = data.memory;
                        document.getElementById("temp").innerHTML = data.temp;
                        document.getElementById("fps").innerHTML = data.fps;
                    });
            }
            setInterval(updateSystemInfo, 1000);
        </script>
    </body>
    </html>
    """
    )


@app.route("/video_feed")
def video_feed():
    """可见光视频流"""
    return Response(
        generate_camera(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


@app.route("/thermal_feed")
def thermal_feed():
    """热成像视频流"""
    return Response(
        generate_thermal(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


@app.route("/system_info")
def system_info():
    """系统信息API"""
    return sys_info


if __name__ == "__main__":
    # 启动后台线程
    threading.Thread(target=capture_thermal_data, daemon=True).start()
    threading.Thread(target=get_system_info, daemon=True).start()
    threading.Thread(target=capture_camera_frames, daemon=True).start()

    # 启动Flask应用
    app.run(host="0.0.0.0", port=5000, threaded=True)
