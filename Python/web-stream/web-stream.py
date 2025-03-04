# 保存为 web_stream.py
import cv2
from flask import Flask, Response
import threading

# 初始化 Flask 应用
app = Flask(__name__)

# 摄像头配置（根据实际设备路径调整）
CAMERA_DEVICE = 0  # 通常为 /dev/video0
FRAME_WIDTH = 640  # 分辨率可调整
FRAME_HEIGHT = 480


# 创建全局摄像头对象（线程安全）
class Camera:
    def __init__(self):
        self.cap = cv2.VideoCapture(CAMERA_DEVICE)
        self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, FRAME_WIDTH)
        self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, FRAME_HEIGHT)
        self.lock = threading.Lock()

    def get_frame(self):
        with self.lock:
            ret, frame = self.cap.read()
            if not ret:
                return None
            # 转换为 JPEG 格式
            ret, jpeg = cv2.imencode(".jpg", frame)
            return jpeg.tobytes()


# 创建全局摄像头实例
camera = Camera()


def generate_frames():
    """视频流生成器"""
    while True:
        frame = camera.get_frame()
        if frame is None:
            break
        # 生成 MJPEG 流
        yield (b"--frame\r\n" b"Content-Type: image/jpeg\r\n\r\n" + frame + b"\r\n")


@app.route("/video_feed")
def video_feed():
    """视频流路由"""
    return Response(
        generate_frames(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


@app.route("/")
def index():
    """主页显示视频播放器"""
    return """
    <html>
      <head>
        <title>OV5647 Live Stream</title>
      </head>
      <body>
        <h1>Live Stream</h1>
        <img src="/video_feed" width="640" height="480">
      </body>
    </html>
    """


if __name__ == "__main__":
    # 检查摄像头是否正常打开
    if not camera.cap.isOpened():
        raise RuntimeError("无法打开摄像头，请检查设备连接和权限！")

    # 启动 Flask 服务器
    app.run(host="0.0.0.0", port=5000, threaded=True)
