"""Local stand-in for jwxt.shmtu.edu.cn: an HTTP CONNECT proxy that terminates TLS with a
throwaway certificate and hands each request to `server.handler`. Never touches the real site."""
import glob
import os
from pathlib import Path
import shutil
import socketserver
import ssl
import subprocess
import threading


def page(token):
    return f'<input value="{token}" name="elecSessionTime" type="hidden">'


class Tunnel(socketserver.StreamRequestHandler):
    def handle(self):
        first = self.rfile.readline()
        if first != b"CONNECT jwxt.shmtu.edu.cn:443 HTTP/1.1\r\n":
            self.server.errors.append(f"unexpected CONNECT: {first!r}")
            return
        while self.rfile.readline().strip():
            pass
        self.wfile.write(b"HTTP/1.1 200 Connection established\r\n\r\n")
        self.wfile.flush()
        with self.server.tls.wrap_socket(self.connection, server_side=True) as connection:
            self.server.handler(connection, self.client_address, self.server)


class Proxy(socketserver.ThreadingTCPServer):
    daemon_threads = True


def find_openssl():
    """openssl may be missing from PATH (e.g. under nix); fall back to the nix store."""
    found = shutil.which("openssl")
    if found:
        return found
    for candidate in sorted(glob.glob("/nix/store/*openssl*/bin/openssl"), reverse=True):
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    raise SystemExit("找不到 openssl（本测试需要它生成临时自签证书）")


def start_proxy(directory, handler):
    """Starts the proxy; returns it and an environment that routes a child process through it."""
    cwd = Path(directory)
    cert, key = cwd / "cert.pem", cwd / "key.pem"
    subprocess.run(
        [find_openssl(), "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
         "-keyout", str(key), "-out", str(cert), "-subj", "/CN=jwxt.shmtu.edu.cn",
         "-addext", "subjectAltName=DNS:jwxt.shmtu.edu.cn",
         "-addext", "basicConstraints=critical,CA:FALSE"],
        check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    proxy = Proxy(("127.0.0.1", 0), Tunnel)
    proxy.handler = handler
    proxy.errors = []
    proxy.tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    proxy.tls.load_cert_chain(cert, key)
    threading.Thread(target=proxy.serve_forever, daemon=True).start()
    env = os.environ.copy()
    for name in ("HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY", "http_proxy", "https_proxy", "all_proxy"):
        env[name] = f"http://127.0.0.1:{proxy.server_address[1]}"
    env.update(NO_PROXY="", no_proxy="", SSL_CERT_FILE=str(cert), SSL_CERT_DIR=directory)
    return proxy, env
