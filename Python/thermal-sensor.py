# thermal_sensor_binary.py
import time
import struct
import sys
import board
import adafruit_mlx90640

# 初始化 MLX90640
i2c = board.I2C()
mlx = adafruit_mlx90640.MLX90640(i2c)
mlx.refresh_rate = adafruit_mlx90640.RefreshRate.REFRESH_2_HZ  # 2Hz 刷新率
frame = [0.0] * 768

try:
    while True:
        # 读取温度数据（类型为 float）
        mlx.getFrame(frame)

        # 打包二进制数据：时间戳 (double) + 768 个温度值 (float)
        # 格式说明：
        #   < : 小端字节序
        #   d : 8字节 double (时间戳)
        #   768f : 768 个 4字节 float
        data_bytes = struct.pack("<d768f", time.time(), *frame)

        # 写入标准输出缓冲区
        sys.stdout.buffer.write(data_bytes)
        sys.stdout.buffer.flush()  # 强制刷新缓冲区

        time.sleep(1)
except KeyboardInterrupt:
    print("程序终止")
