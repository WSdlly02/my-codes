import socket
import subprocess

def start_service():
    # 执行系统命令
    subprocess.run(['echo', '22024'], check=True)

def main():
    # 创建一个IPv4和IPv6兼容的套接字
    with socket.socket(socket.AF_INET6, socket.SOCK_STREAM) as s:
        # 绑定到所有网络接口的12024端口
        s.bind(('::', 22024))
        s.listen()
        print("Listening on port 22024 (IPv4 & IPv6)...")
        
        while True:
            # 接受连接
            conn, addr = s.accept()
            with conn:
                print('Connected by', addr)
                start_service()

if __name__ == "__main__":
    main()
