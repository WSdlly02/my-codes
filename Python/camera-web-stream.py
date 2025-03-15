from picamera2 import Picamera2
from flask import Flask, Response
import cv2
import numpy as np
import threading
import time
import psutil

app = Flask(__name__)

# 高级摄像头配置
picam2 = Picamera2()
config = picam2.create_video_configuration(
    main={"size": (2592, 1944)},  # 原生分辨率
    lores={"size": (2592, 1944)},  # 预览流
    display="main",
    encode="main",
    controls={"FrameRate": 30},
)
picam2.configure(config)
picam2.start()

frame_lock = threading.Lock()
latest_frame = None


def adaptive_quality():
    """动态质量调整"""
    cpu = psutil.cpu_percent()
    net = psutil.net_io_counters().bytes_sent
    return 90 if cpu < 70 and net < 1e6 else 75


def capture_frames():
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


def generate_stream():
    """生成 MJPEG 流"""
    while True:
        if latest_frame:
            yield (
                b"--frame\r\n"
                b"Content-Type: image/jpeg\r\n\r\n" + latest_frame + b"\r\n\r\n"
            )


@app.route("/video_feed")
def video_feed():
    """视频流路由"""
    return Response(
        generate_stream(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


@app.route("/")
def index():
    """显示监控页面"""
    return """
    <html>
      <head>
        <title>树莓派摄像头实时监控</title>
        <style>
          body { margin: 0; background: #000; }
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
    # 启动摄像头捕获线程
    capture_thread = threading.Thread(target=capture_frames, daemon=True)
    capture_thread.start()

    # 启动 Web 服务器
    app.run(host="0.0.0.0", port=5000, threaded=True)
