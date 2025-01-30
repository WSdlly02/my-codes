import socket

def forward_data(data, target_host, target_port):
    try:
        # 创建一个用于发送数据的套接字
        with socket.socket(socket.AF_INET6, socket.SOCK_STREAM) as forward_sock:
            forward_sock.connect((target_host, target_port))
            forward_sock.sendall(data)
            print(f"Data forwarded to {target_host}:{target_port}")
    except ConnectionResetError:
        print(f"Connection to {target_host}:{target_port} was reset by peer.")
    except Exception as e:
        print(f"An error occurred while forwarding data: {e}")

def main():
    listen_port = 12024
    forward_host = '::'  # 将数据包转发到的目标主机(支持IPv4和IPv6)
    forward_port = 22024

    # 创建一个IPv4和IPv6兼容的套接字
    with socket.socket(socket.AF_INET6, socket.SOCK_STREAM) as listen_sock:
        listen_sock.bind(('::', listen_port))
        listen_sock.listen()
        print(f"Listening on port {listen_port} (IPv4 & IPv6)...")
        
        while True:
            conn, addr = listen_sock.accept()
            with conn:
                print('Connected by', addr)
                try:
                    data = conn.recv(1024)
                    if data:
                        print("Data received: ", data)
                        forward_data(data, forward_host, forward_port)
                except Exception as e:
                    print(f"An error occurred while receiving data: {e}")

if __name__ == "__main__":
    main()
