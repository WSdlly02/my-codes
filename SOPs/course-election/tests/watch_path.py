#!/usr/bin/env python3
"""`watch`（事件驱动捡漏）的离线契约测试：本地 TLS CONNECT 打桩，不访问真实教务系统。

验证三件事：
  1. 启动时只 GET 一次 defaultPage —— 既建立服务端课选上下文，又取到 token；
  2. 名额满员期间只读地轮询 queryStdCount，**一次 POST 都不发**；
  3. 名额出现空位时才 POST，且用的是启动时那个 token；成功后自动停止。

用法: python3 tests/watch_path.py target/release/course-election
"""
import collections
import http.server
import os
import pty
import select
import socketserver
import ssl
import subprocess
import sys
import tempfile
import threading
import time
import urllib.parse
from pathlib import Path

TOKEN = "20260922125615"
LESSON = "252282"
FULL = "window.lessonId2Counts={'252282':{sc:92,lc:92,wc:0}};"
VACANT = "window.lessonId2Counts={'252282':{sc:91,lc:92,wc:0}};"


def page(token):
    return f'<input value="{token}" name="elecSessionTime" type="hidden">'


class Reply(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_GET(self):
        self.reply()

    def do_POST(self):
        self.reply()

    def reply(self):
        parsed = urllib.parse.urlsplit(self.path)
        body = self.rfile.read(int(self.headers.get("Content-Length", "0"))).decode()
        server = self.server
        status = 200
        try:
            if self.command == "POST":
                query = urllib.parse.parse_qs(parsed.query)
                token = query.get("elecSessionTime", [""])[0]
                server.log.append(("POST", "batchOperator", token))
                assert parsed.path == "/shmtu/stdElectCourse!batchOperator.action", parsed.path
                assert query.get("profileId") == ["3112"], query
                assert token == TOKEN, f"POST 用了 {token!r}，期望启动时取到的 {TOKEN!r}"
                assert urllib.parse.parse_qs(body) == {"operator0": [f"{LESSON}:true:0"]}, body
                # 只有真的出现空位才应该走到这里
                assert server.mode == "vacant", "名额还满着就发起了 POST"
                data = "选课成功"
                if server.stale:
                    server.stale -= 1
                    server.page_delay = 2
                    data = "选课失败:同时打开多个选课页面，请至最新页面进行操作"
                time.sleep(server.post_delay)
            elif parsed.path == "/shmtu/stdElectCourse!defaultPage.action":
                server.log.append(("GET", "defaultPage", ""))
                assert parsed.query == "electionProfile.id=3112", parsed.query
                data = page(TOKEN)
                time.sleep(server.page_delay)
            elif parsed.path == "/shmtu/stdElectCourse!queryStdCount.action":
                server.log.append(("GET", "queryStdCount", ""))
                assert parsed.query == "profileId=3112", parsed.query
                data = FULL if server.mode == "full" else VACANT
                time.sleep(server.query_delay)
                if server.query_failures:
                    server.query_failures -= 1
                    status = server.query_status
                    data = "temporary unavailable"
            else:
                raise AssertionError(f"未预期的请求 {self.command} {parsed.path}")
        except AssertionError as error:
            server.errors.append(str(error))
            data = "unexpected request"
        raw = data.encode()
        self.send_response_only(status)
        # Date 与页面 token 故意不同：token 必须来自 HTML
        self.send_header("Date", "Tue, 22 Sep 2026 04:56:16 GMT")
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(raw)))
        self.send_header("Connection", "close")
        self.end_headers()
        try:
            self.wfile.write(raw)
        except (BrokenPipeError, ConnectionResetError, ssl.SSLError):
            pass  # Read-only deadline cancellation can close the connection.


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
            Reply(connection, self.client_address, self.server)


class Proxy(socketserver.ThreadingTCPServer):
    daemon_threads = True


def find_openssl():
    """openssl 不一定在 PATH 上（比如 nix 环境），找不到就回退到 nix store。"""
    import glob
    import shutil
    found = shutil.which("openssl")
    if found:
        return found
    for candidate in sorted(glob.glob("/nix/store/*openssl*/bin/openssl"), reverse=True):
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    raise SystemExit("找不到 openssl（本测试需要它生成临时自签证书）")


def main():
    binary = str(Path(sys.argv[1] if len(sys.argv) > 1 else "target/release/course-election").resolve())
    openssl = find_openssl()
    with tempfile.TemporaryDirectory() as cwd, Proxy(("127.0.0.1", 0), Tunnel) as proxy:
        cert, key = Path(cwd) / "cert.pem", Path(cwd) / "key.pem"
        subprocess.run([
            openssl, "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
            "-keyout", str(key), "-out", str(cert), "-subj", "/CN=jwxt.shmtu.edu.cn",
            "-addext", "subjectAltName=DNS:jwxt.shmtu.edu.cn",
            "-addext", "basicConstraints=critical,CA:FALSE",
        ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        proxy.tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        proxy.tls.load_cert_chain(cert, key)
        proxy.log, proxy.errors, proxy.mode = [], [], "full"
        proxy.query_failures = proxy.stale = 0
        proxy.query_status = 503
        proxy.query_delay = proxy.page_delay = proxy.post_delay = 0
        threading.Thread(target=proxy.serve_forever, daemon=True).start()

        env = os.environ.copy()
        for name in ("HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY", "http_proxy", "https_proxy", "all_proxy"):
            env[name] = f"http://127.0.0.1:{proxy.server_address[1]}"
        env.update(NO_PROXY="", no_proxy="", SSL_CERT_FILE=str(cert), SSL_CERT_DIR=cwd)

        master, slave = pty.openpty()
        proc = subprocess.Popen([binary], cwd=cwd, env=env, stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        output = bytearray()

        def wait(text, start=0, timeout=20):
            deadline = time.monotonic() + timeout
            while text.encode() not in output[start:]:
                remaining = deadline - time.monotonic()
                assert remaining > 0, f"等待 {text!r} 超时:\n{output[start:].decode(errors='replace')}"
                if select.select([master], [], [], remaining)[0]:
                    output.extend(os.read(master, 65536))

        def prompt_after(text, mark):
            wait(text, mark)
            wait("course-election>", output.index(text.encode(), mark) + len(text.encode()))

        def start_watch(command):
            mark = len(output)
            before = len(proxy.log)
            os.write(master, (command + '\r').encode())
            return mark, before

        try:
            wait("course-election>")
            for cmd, expect in (("profile 3112", "profile=3112"), (f"target {LESSON}", f"target={LESSON}")):
                mark = len(output)
                os.write(master, (cmd + "\r").encode())
                prompt_after(expect, mark)

            # ---- 用例 1：满员期间只轮询、不下单；出现空位才 POST ----
            mark = len(output)
            os.write(master, b"watch 1 10\r")
            wait("第 1 轮", mark)
            assert proxy.mode == "full"
            assert not any(entry[0] == "POST" for entry in proxy.log), proxy.log
            print("PASS: 满员期间只轮询 queryStdCount，没有发起任何 POST")

            proxy.mode = "vacant"
            wait("选课成功", mark)
            wait("已命中", mark)
            prompt_after("已命中", mark)
            kinds = [entry[1] for entry in proxy.log]
            assert kinds.count("defaultPage") == 1, kinds
            assert kinds.count("batchOperator") == 1, kinds
            assert kinds.count("queryStdCount") >= 2, kinds
            assert not proxy.errors, proxy.errors
            print(f"PASS: 空位出现后用启动时的 token 抢到（defaultPage×1, queryStdCount×{kinds.count('queryStdCount')}, POST×1）")

            # ---- 用例 2：--dry-run 只观察不出手 ----
            before = len(proxy.log)
            mark = len(output)
            os.write(master, b"watch 1 3 --dry-run\r")
            wait("dry-run 不出手", mark)
            assert not any(entry[0] == "POST" for entry in proxy.log[before:]), proxy.log[before:]
            print("PASS: --dry-run 发现空位但不出手")
            wait("达到最长等待时间", mark)
            prompt_after("达到最长等待时间", mark)

            # Deadline shorter than poll interval: no extra poll or write after sleep.
            proxy.mode = "full"
            mark, before = start_watch("watch 3 1")
            wait("第 1 轮", mark)
            proxy.mode = "vacant"
            prompt_after("达到最长等待时间", mark)
            assert [e[1] for e in proxy.log[before:]] == ["defaultPage", "queryStdCount"]
            print("PASS: 轮询间隔超过剩余时间时，不越过截止时间提交")

            # Slow count response would show vacancy only after the deadline.
            proxy.query_delay = 2
            mark, before = start_watch("watch 1 1")
            prompt_after("达到最长等待时间", mark)
            assert not any(e[0] == "POST" for e in proxy.log[before:])
            time.sleep(1.2)  # Let the cancelled read finish in the stub.
            proxy.query_delay = 0
            print("PASS: 慢查询耗尽期限不会触发 POST")

            # Exhaust the GET helper's three retries, then recover next watch round.
            proxy.query_failures = 3
            mark, before = start_watch("watch 1 8")
            wait("名额查询暂时失败", mark)
            prompt_after("已命中", mark)
            assert [e[1] for e in proxy.log[before:]].count("defaultPage") == 1
            assert [e[1] for e in proxy.log[before:]].count("batchOperator") == 1
            print("PASS: 临时查询错误耗尽 GET 重试后继续监视并恢复")

            proxy.query_status, proxy.query_failures = 401, 1
            mark, before = start_watch("watch 1 8")
            prompt_after("错误：", mark)
            assert [e[1] for e in proxy.log[before:]] == ["defaultPage", "queryStdCount"]
            proxy.query_status = 503
            print("PASS: 明确认证错误退出，不盲目重试")

            # Initial defaultPage is part of the same total deadline.
            proxy.page_delay = 2
            mark, before = start_watch("watch 1 1")
            prompt_after("达到最长等待时间", mark)
            assert [e[1] for e in proxy.log[before:]] == ["defaultPage"]
            time.sleep(1.2)
            proxy.page_delay = 0
            print("PASS: 初始化计入总期限")

            # Stale rejection followed by slow token refresh: no second POST.
            proxy.stale = 1
            mark, before = start_watch("watch 1 2")
            prompt_after("达到最长等待时间", mark)
            assert [e[1] for e in proxy.log[before:]].count("batchOperator") == 1
            assert [e[1] for e in proxy.log[before:]].count("defaultPage") == 2
            time.sleep(1.2)
            proxy.page_delay = 0
            print("PASS: token 刷新耗尽期限后不再 POST")

            # A write started in time must still report its eventual result.
            proxy.post_delay = 2
            mark, before = start_watch("watch 1 1")
            prompt_after("已命中", mark)
            assert [e[1] for e in proxy.log[before:]].count("batchOperator") == 1
            proxy.post_delay = 0
            print("PASS: 已发出的 POST 不因监视到期而取消")

            os.write(master, b"quit\r")
            proc.wait(timeout=5)
            assert proc.returncode == 0, proc.returncode
            assert not proxy.errors, proxy.errors
            print("PASS: watch 契约测试全部通过")
        finally:
            if proc.poll() is None:
                proc.terminate()
                proc.wait(timeout=5)
            os.close(master)
            proxy.shutdown()


if __name__ == "__main__":
    main()
