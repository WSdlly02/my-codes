from flask import Flask, render_template, Response, request
import subprocess
import os
import time

app = Flask(__name__)

# 模拟警报状态
alarm_status = False


# 实时视频流生成器
def generate_frames():
    from picamera import PiCamera

    camera = PiCamera()
    camera.resolution = (640, 480)
    camera.framerate = 24
    time.sleep(2)  # 让摄像头预热

    while True:
        frame = camera.capture("static/frame.jpg", use_video_port=True)
        with open("static/frame.jpg", "rb") as f:
            frame = f.read()
        yield (b"--frame\r\n" b"Content-Type: image/jpeg\r\n\r\n" + frame + b"\r\n")


# 主页路由
@app.route("/")
def index():
    return render_template("index.html")


# 视频流路由
@app.route("/video_feed")
def video_feed():
    return Response(
        generate_frames(), mimetype="multipart/x-mixed-replace; boundary=frame"
    )


# 重启树莓派路由
@app.route("/reboot", methods=["POST"])
def reboot():
    subprocess.call(["sudo", "reboot"])
    return "Rebooting..."


# 查看系统信息路由
@app.route("/system_info")
def system_info():
    info = subprocess.check_output(["uname", "-a"]).decode("utf-8")
    return info


# 控制警报路由
@app.route("/alarm", methods=["POST"])
def alarm():
    global alarm_status
    alarm_status = not alarm_status
    if alarm_status:
        # 这里可以添加触发警报的代码，例如播放声音或发送通知
        return "Alarm ON"
    else:
        # 这里可以添加关闭警报的代码
        return "Alarm OFF"


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000, threaded=True)
