# thermal_web_stream.py
from picamera2 import Picamera2
from flask import Flask, Response
import cv2
import threading
import time

# 初始化 Flask
app = Flask(__name__)

# 摄像头配置
picam2 = Picamera2()
config = picam2.create_preview_configuration(
    main={"size": (640, 480), "format": "RGB888"}
)
picam2.configure(config)
picam2.start()

# 全局帧缓存和锁
frame_lock = threading.Lock()
latest_frame = None


def capture_frames():
    """持续捕获摄像头帧"""
    global latest_frame
    while True:
        with frame_lock:
            # 从摄像头获取帧并转换为 OpenCV 格式
            frame = picam2.capture_array()
            frame = cv2.cvtColor(frame, cv2.COLOR_RGB2BGR)
            _, jpeg = cv2.imencode(".jpg", frame)
            latest_frame = jpeg.tobytes()
        time.sleep(0.03)  # 约 30 FPS


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
