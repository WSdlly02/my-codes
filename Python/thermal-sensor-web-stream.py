# thermal_stream.py
import time
import board
import numpy as np
import cv2
from flask import Flask, Response
import adafruit_mlx90640
from threading import Lock

# 初始化摄像头
i2c = board.I2C()
mlx = adafruit_mlx90640.MLX90640(i2c)
mlx.refresh_rate = adafruit_mlx90640.RefreshRate.REFRESH_8_HZ

# 全局变量
frame = [0] * 768
thermal_lock = Lock()
app = Flask(__name__)

# 颜色映射配置
COLORMAP = cv2.COLORMAP_JET  # 使用OpenCV的Jet色图
INTERPOLATION = cv2.INTER_CUBIC  # 插值算法
SCALE_FACTOR = 10  # 图像放大倍数 (32x24 -> 320x240)


def capture_thread():
    """持续读取温度数据的线程"""
    global frame
    while True:
        try:
            with thermal_lock:
                mlx.getFrame(frame)
        except ValueError:
            continue  # 忽略偶发的I2C错误
        time.sleep(1 / 8)  # 匹配8Hz刷新率


def process_frame():
    """处理温度数据生成彩色图像"""
    with thermal_lock:
        # 转换为numpy数组
        data = np.array(frame).reshape((24, 32)).astype(np.float32)

        # 归一化到0-255
        min_temp = np.min(data)
        max_temp = np.max(data)
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
        if max_temp > 50:  # 高温报警阈值
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


def generate():
    """生成MJPEG视频流"""
    while True:
        frame = process_frame()
        ret, jpeg = cv2.imencode(".jpg", frame)
        yield (
            b"--frame\r\n"
            b"Content-Type: image/jpeg\r\n\r\n" + jpeg.tobytes() + b"\r\n\r\n"
        )


@app.route("/video_feed")
def video_feed():
    """视频流路由"""
    return Response(generate(), mimetype="multipart/x-mixed-replace; boundary=frame")


@app.route("/")
def index():
    """显示页面"""
    return """
    <html>
      <head>
        <title>MLX90640 热成像实时监控</title>
        <style>
          body { background: #000; margin: 0; }
          img { 
            width: 100vw; 
            height: 100vh; 
            object-fit: contain;
          }
        </style>
      </head>
      <body>
        <img src="/video_feed">
      </body>
    </html>
    """


if __name__ == "__main__":
    # 启动数据采集线程
    import threading

    threading.Thread(target=capture_thread, daemon=True).start()

    # 启动Web服务器
    app.run(host="0.0.0.0", port=5000, threaded=True)
