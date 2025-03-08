# web_stream_fixed.py
import cv2
from flask import Flask, Response
import threading
import logging

# 配置日志（调试用）
logging.basicConfig(level=logging.INFO)

# 初始化 Flask
app = Flask(__name__)

# 摄像头配置
CAMERA_DEVICE = "/dev/media0"  # 明确指定设备路径
FRAME_WIDTH = 640
FRAME_HEIGHT = 480


# 摄像头类（带错误处理）
class Camera:
    def __init__(self):
        self.cap = cv2.VideoCapture(CAMERA_DEVICE, cv2.CAP_V4L2)
        if not self.cap.isOpened():
            raise RuntimeError(f"无法打开摄像头 {CAMERA_DEVICE}")
        self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, FRAME_WIDTH)
        self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, FRAME_HEIGHT)
        self.cap.set(cv2.CAP_PROP_FPS, 15)
        self.lock = threading.Lock()

    def get_frame(self):
        with self.lock:
            try:
                ret, frame = self.cap.read()
                if not ret:
                    logging.error("摄像头读取失败")
                    return None
                ret, jpeg = cv2.imencode(".jpg", frame)
                if not ret:
                    logging.error("JPEG 编码失败")
                    return None
                return jpeg.tobytes()
            except Exception as e:
                logging.error(f"捕获帧错误: {e}")
                return None


# 初始化摄像头
try:
    camera = Camera()
except Exception as e:
    logging.critical(f"摄像头初始化失败: {e}")
    exit(1)


def generate_frames():
    while True:
        frame = camera.get_frame()
        if frame:
            yield (b"--frame\r\n" b"Content-Type: image/jpeg\r\n\r\n" + frame + b"\r\n")


@app.route("/video_feed")
def video_feed():
    return Response(
        generate_frames(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


@app.route("/")
def index():
    return """
    <html>
      <head><title>OV5647 实时画面</title></head>
      <body>
        <h1>OV5647 实时画面</h1>
        <img src="/video_feed" width="640" height="480">
      </body>
    </html>
    """


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000, threaded=True)
