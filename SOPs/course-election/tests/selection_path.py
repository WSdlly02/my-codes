"""Offline REPL contract tests using a local TLS CONNECT stub (requires openssl).

No upstream forwarding, real credentials, cookies or course operations.
Usage: python3 tests/selection_path.py target/debug/course-election

覆盖"已打开的选课页面"语义：首个写操作打开页面，后续命令复用；页面过期后
由"下一次允许的尝试"重开；换轮次/登录失效使页面作废；refresh 顺带回填页面。
"""
import collections
import datetime
import http.server
import os
from pathlib import Path
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


A, B = '20260922125615', '20260922125617'
STALE = '选课失败:同时打开多个选课页面，请至最新页面进行操作'


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
        query = urllib.parse.parse_qs(parsed.query)
        body = self.rfile.read(int(self.headers.get('Content-Length', '0'))).decode()
        server = self.server
        is_post = self.command == 'POST'
        token = query.get('elecSessionTime', [''])[0] if is_post else ''
        with server.lock:
            try:
                if is_post:
                    assert parsed.path == '/shmtu/stdElectCourse!batchOperator.action', parsed.path
                    assert query.get('profileId', [''])[0].isdigit(), query
                    operator = '252279:false' if token == 'undefined' else '252279:true:0'
                    assert urllib.parse.parse_qs(body) == {'operator0': [operator]}, body
                    server.seen.append(('POST', token))
                    method, want_token, response = server.steps.popleft()
                    assert (method, want_token) == ('POST', token), (token, want_token)
                elif parsed.path == '/shmtu/stdElectCourse!defaultPage.action':
                    assert query.get('electionProfile.id', [''])[0].isdigit(), query
                    server.seen.append(('GET', 'defaultPage'))
                    method, want_token, response = server.steps.popleft()
                    assert (method, want_token) == ('GET', ''), (method, want_token)
                elif parsed.path == '/shmtu/stdElectCourse!data.action':
                    server.seen.append(('GET', 'data'))
                    response = getattr(server, 'lesson_payload', 'var lessonJSONs = [];')
                elif parsed.path == '/shmtu/stdElectCourse!queryStdCount.action':
                    server.seen.append(('GET', 'counts'))
                    response = 'window.lessonId2Counts = {};'
                elif parsed.path == '/shmtu/stdElectCourse.action':
                    server.seen.append(('GET', 'entry'))
                    response = 'entry'
                else:
                    raise AssertionError(f'unexpected {self.command} {parsed.path}')
            except (AssertionError, IndexError, KeyError) as error:
                server.errors.append(repr(error))
                response = 'unexpected request'
        data = response.encode()
        # Deliberately unrelated to HTML token: Date must never supply the token.
        self.send_response_only(200)
        self.send_header('Date', 'Tue, 22 Sep 2026 04:56:16 GMT')
        self.send_header('Content-Type', 'text/html; charset=utf-8')
        self.send_header('Content-Length', str(len(data)))
        self.send_header('Connection', 'close')
        self.end_headers()
        self.wfile.write(data)


class Tunnel(socketserver.StreamRequestHandler):
    def handle(self):
        first = self.rfile.readline()
        if first != b'CONNECT jwxt.shmtu.edu.cn:443 HTTP/1.1\r\n':
            self.server.errors.append(f'unexpected CONNECT: {first!r}')
            return
        while self.rfile.readline().strip():
            pass
        self.wfile.write(b'HTTP/1.1 200 Connection established\r\n\r\n')
        self.wfile.flush()
        with self.server.tls.wrap_socket(self.connection, server_side=True) as connection:
            Reply(connection, self.client_address, self.server)


class Proxy(socketserver.ThreadingTCPServer):
    daemon_threads = True


def find_openssl():
    """openssl 不一定在 PATH 上（比如 nix 环境），找不到就回退到 nix store。"""
    import glob
    import shutil
    found = shutil.which('openssl')
    if found:
        return found
    for candidate in sorted(glob.glob('/nix/store/*openssl*/bin/openssl'), reverse=True):
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    raise SystemExit('找不到 openssl（本测试需要它生成临时自签证书）')


def main():
    binary = str(Path(sys.argv[1]).resolve())
    openssl = find_openssl()
    with tempfile.TemporaryDirectory() as cwd, Proxy(('127.0.0.1', 0), Tunnel) as proxy:
        cert, key = Path(cwd) / 'cert.pem', Path(cwd) / 'key.pem'
        subprocess.run([
            openssl, 'req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-days', '1',
            '-keyout', str(key), '-out', str(cert), '-subj', '/CN=jwxt.shmtu.edu.cn',
            '-addext', 'subjectAltName=DNS:jwxt.shmtu.edu.cn',
            '-addext', 'basicConstraints=critical,CA:FALSE',
        ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        proxy.tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        proxy.tls.load_cert_chain(cert, key)
        proxy.steps, proxy.seen, proxy.errors = collections.deque(), [], []
        proxy.lock = threading.Lock()
        threading.Thread(target=proxy.serve_forever, daemon=True).start()
        env = os.environ.copy()
        for name in ('HTTP_PROXY', 'HTTPS_PROXY', 'ALL_PROXY', 'http_proxy', 'https_proxy', 'all_proxy'):
            env[name] = f'http://127.0.0.1:{proxy.server_address[1]}'
        env.update(NO_PROXY='', no_proxy='', SSL_CERT_FILE=str(cert), SSL_CERT_DIR=cwd)
        master, slave = pty.openpty()
        proc = subprocess.Popen([binary], cwd=cwd, env=env, stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        output = bytearray()

        def wait(text, start=0):
            deadline = time.monotonic() + 20
            while text.encode() not in output[start:]:
                remaining = deadline - time.monotonic()
                assert remaining > 0, output[start:].decode(errors='replace')
                if select.select([master], [], [], remaining)[0]:
                    output.extend(os.read(master, 65536))

        def command(text, result):
            mark = len(output)
            os.write(master, (text + '\r').encode())
            if result == 'course-election>':
                result = f'course-election> {text}\r\n'
            wait(result, mark)
            result_end = output.index(result.encode(), mark) + len(result.encode())
            wait('course-election>', result_end)

        def case(label, commands, steps, background=()):
            """commands: [(命令, 期望输出), ...]；steps 还会被严格按序消费（顺序由它保证），
            background 用于 refresh 这类会额外拉 data/counts、且并发顺序不定的请求。"""
            assert not proxy.steps
            start = len(proxy.seen)
            proxy.steps.extend(steps)
            for text, expected in commands:
                command(text, expected)
            assert not proxy.errors, proxy.errors
            assert not proxy.steps, list(proxy.steps)
            # seen 里 GET 记录为 ('GET','defaultPage')，步骤表里 GET 的 token 位是 ''
            expected = [('GET', 'defaultPage') if m == 'GET' else ('POST', t) for m, t, _ in steps]
            assert sorted(proxy.seen[start:]) == sorted(expected + list(background)), proxy.seen[start:]
            print('PASS:', label)

        try:
            wait('course-election>')
            command('profile 3112', 'profile=3112')
            command('target 252279', 'target=252279')

            case('首个写操作打开页面，普通失败复用同一 HTML token（忽略 Date）', [
                ('fire 3 0', '[3] 选课成功'),
            ], [
                ('GET', '', page(A)), ('POST', A, '选课失败:人数已满'),
                ('POST', A, '选课失败:人数已满'), ('POST', A, '选课成功'),
            ])

            case('后续命令复用已打开的页面：只有 POST，不再 GET defaultPage', [
                ('fire 1 0', '达到最大尝试次数'),
            ], [
                ('POST', A, '选课失败:人数已满'),
            ])

            case('drop 复用同一页面，且仍发送 undefined', [
                ('drop 1 0', '[1] 退课成功'),
            ], [
                ('POST', 'undefined', '退课成功'),
            ])

            case('页面被判过期：单次尝试不额外重开页面', [
                ('fire 1 0', '达到最大尝试次数'),
            ], [
                ('POST', A, STALE),
            ])

            case('过期后下一次允许的尝试重开页面并取到新 token', [
                ('fire 1 0', '[1] 选课成功'),
            ], [
                ('GET', '', page(B)), ('POST', B, '选课成功'),
            ])

            case('refresh 打开的新页面被后续 fire 复用（不重复 GET）', [
                ('refresh', 'mapping=0 counts=0'),
                ('fire 1 0', '[1] 选课成功'),
            ], [
                ('GET', '', page(A)), ('POST', A, '选课成功'),
            ], background=[('GET', 'data'), ('GET', 'counts')])

            case('换轮次使页面作废：之后的写操作重新打开页面', [
                ('profile 3113', 'profile=3113'),
                ('target 252279', 'target=252279'),
                ('drop 1 0', '[1] 退课成功'),
            ], [
                ('GET', '', page(A)), ('POST', 'undefined', '退课成功'),
            ])

            case('页面缺少 token 时不提交（先换轮次作废旧页面）', [
                ('profile 3112', 'profile=3112'),
                ('target 252279', 'target=252279'),
                ('fire 1 0', '达到最大尝试次数'),
            ], [
                ('GET', '', '<html>登录失效</html>'),
            ])

            case('无上下文时直接 drop 必须先初始化', [
                ('drop 1 0', '[1] 退课成功'),
            ], [('GET', '', page(B)), ('POST', 'undefined', '退课成功')])

            proxy.lesson_payload = 'invalid lesson payload'
            case('refresh 后续数据解析失败仍保留新 token', [
                ('refresh', '错误：'), ('fire 1 0', '[1] 选课成功'),
            ], [('GET', '', page(A)), ('POST', A, '选课成功')],
                background=[('GET', 'data'), ('GET', 'counts')])
            proxy.lesson_payload = 'var lessonJSONs = [];'

            case('refresh 页面解析失败清除旧 token，后续写入必须重开', [
                ('refresh', '错误：'), ('fire 1 0', '[1] 选课成功'),
            ], [('GET', '', '<html>bad page</html>'),
                ('GET', '', page(B)), ('POST', B, '选课成功')])

            case('已选查询更新上下文，不靠 REPL 手动回填', [
                ('find --selected', 'course-election>'), ('fire 1 0', '[1] 选课成功'),
            ], [('GET', '', page(A)), ('POST', A, '选课成功')])

            case('已选页面加载失败也清除旧 token', [
                ('find --selected', '错误：'), ('drop 1 0', '[1] 退课成功'),
            ], [('GET', '', '<html>bad page</html>'),
                ('GET', '', page(B)), ('POST', 'undefined', '退课成功')])

            case('初始化失败不丢失 fire 剩余尝试额度', [
                ('profile 3112', 'profile=3112'), ('target 252279', 'target=252279'),
                ('fire 2 0', '[2] 选课成功'),
            ], [('GET', '', '<html>bad page</html>'),
                ('GET', '', page(A)), ('POST', A, '选课成功')])

            target = datetime.datetime.now(datetime.timezone.utc) + datetime.timedelta(seconds=2)
            case('定时 arm 提前准备复用页面，保留两次尝试而不新增 GET', [
                ('arm ' + target.isoformat(), '[2] 选课成功'),
            ], [('POST', A, '选课失败:人数已满'), ('POST', A, '选课成功')],
                background=[('GET', 'entry')])

            os.write(master, b'quit\r')
            proc.wait(timeout=5)
            assert proc.returncode == 0
        finally:
            if proc.poll() is None:
                proc.terminate()
                proc.wait(timeout=5)
            os.close(master)
            proxy.shutdown()


if __name__ == '__main__':
    main()
