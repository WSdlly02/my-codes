"""
picamera2 功能测试脚本
功能验证顺序：
1. 库导入检查
2. 摄像头硬件检测
3. 基本配置初始化
4. 静态图像捕获
5. 实时预览测试
"""

import sys
import time
import subprocess
from textwrap import dedent


def print_step(title):
    print(f"\n\033[1;34m== 正在测试: {title} ==\033[0m")


def print_success(msg):
    print(f"\033[1;32m✓ {msg}\033[0m")


def print_warning(msg):
    print(f"\033[1;33m⚠️ {msg}\033[0m")


def print_error(msg):
    print(f"\033[1;31m✗ {msg}\033[0m")


def check_libcamera():
    """检查 libcamera 是否安装"""
    print_step("libcamera 基础支持")
    try:
        result = subprocess.run(
            ["libcamera-hello", "--list-cameras"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if "No cameras available" in result.stdout:
            print_error("未检测到摄像头硬件")
            print("建议操作:")
            print("1. 检查摄像头排线连接")
            print("2. 确认已启用摄像头接口")
            print("3. 更新系统固件: sudo rpi-update")
            return False
        else:
            print_success("libcamera 工作正常")
            print(f"检测到摄像头信息:\n{result.stdout}")
            return True
    except FileNotFoundError:
        print_error("未找到 libcamera 组件")
        print("安装命令:")
        print("Raspbian: sudo apt install libcamera-apps")
        print("NixOS: nix-env -iA nixos.libcamera")
        return False


def test_picamera2():
    """执行 picamera2 功能测试"""
    print_step("1. 导入 picamera2 库")
    try:
        from picamera2 import Picamera2, Preview
    except ImportError as e:
        print_error(f"无法导入 picamera2: {e}")
        print("安装命令:")
        print("Raspbian: sudo apt install python3-picamera2")
        print("NixOS: nix-env -iA nixos.python3Packages.picamera2")
        return False
    print_success("库导入成功")

    print_step("2. 检测可用摄像头")
    try:
        camera_info = Picamera2.global_camera_info()
        if not camera_info:
            print_error("未检测到任何摄像头")
            return False
        print_success(f"找到 {len(camera_info)} 个摄像头")
        for idx, info in enumerate(camera_info):
            print(f"Camera {idx}: {info}")
    except Exception as e:
        print_error(f"摄像头检测失败: {e}")
        return False

    print_step("3. 初始化摄像头")
    try:
        picam2 = Picamera2(0)  # 强制使用第一个摄像头
        config = picam2.create_preview_configuration(
            main={"size": (640, 480)}, raw={"size": picam2.sensor_resolution}
        )
        picam2.configure(config)
        picam2.start()
        time.sleep(2)  # 等待摄像头初始化
    except Exception as e:
        print_error(f"摄像头初始化失败: {e}")
        print("可能原因:")
        print(
            dedent(
                """
        - 摄像头被其他进程占用
        - 内核驱动未正确加载
        - 硬件不兼容
        """
            )
        )
        return False
    print_success("摄像头初始化成功")

    print_step("4. 捕获测试图像")
    try:
        test_image = picam2.capture_image("main")
        test_image.save("test_image.jpg")
        print_success("图像已保存为 test_image.jpg")
    except Exception as e:
        print_error(f"图像捕获失败: {e}")
        return False

    print_step("5. 实时预览测试")
    try:
        picam2.start_preview(Preview.QTGL)
        print_success("预览窗口已打开，等待 5 秒...")
        time.sleep(5)
        picam2.stop_preview()
    except Exception as e:
        print_error(f"预览失败: {e}")
        print("可能原因:")
        print("- 无图形界面环境")
        print("- OpenGL 驱动问题")
        return False

    print_step("6. 清理资源")
    try:
        picam2.close()
        print_success("摄像头已正确释放")
    except Exception as e:
        print_error(f"资源释放失败: {e}")
        return False

    return True


def check_user_permissions():
    """检查用户权限"""
    print_step("用户权限验证")
    try:
        with open("/dev/video0", "rb") as f:
            pass
        print_success("摄像头设备访问权限正常")
    except PermissionError:
        print_error("权限不足，无法访问 /dev/video0")
        print("解决命令:")
        print("sudo usermod -aG video $USER")
        print("注销后重新登录生效")
        return False
    return True


if __name__ == "__main__":
    print("\033[1;36m=== 开始 picamera2 功能测试 ===\033[0m")

    # 前置检查
    if not check_user_permissions():
        sys.exit(1)

    if not check_libcamera():
        sys.exit(1)

    # 主测试流程
    if test_picamera2():
        print("\n\033[1;42m 所有测试通过! \033[0m")
    else:
        print("\n\033[1;41m 测试未通过，请检查上述错误 \033[0m")
        sys.exit(1)
