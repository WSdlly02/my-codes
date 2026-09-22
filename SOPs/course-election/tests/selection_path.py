"""Offline REPL contract tests using a local TLS CONNECT stub (requires openssl).

No upstream forwarding, real credentials, cookies or course operations.
Usage: python3 tests/selection_path.py target/debug/course-election
"""
import collections
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
        is_post = self.command == 'POST'
        actual = ('POST', query.get('elecSessionTime', [''])[0]) if is_post else ('GET', '')
        self.server.seen.append(actual)
        try:
            method, token, response = self.server.steps.popleft()
            assert actual == (method, token), (actual, (method, token))
            if is_post:
                assert parsed.path == '/shmtu/stdElectCourse!batchOperator.action'
                assert query['profileId'] == ['3112']
                operator = '252279:false' if token == 'undefined' else '252279:true:0'
                assert urllib.parse.parse_qs(body) == {'operator0': [operator]}
            else:
                assert parsed.path == '/shmtu/stdElectCourse!defaultPage.action'
                assert query['electionProfile.id'] == ['3112']
        except (AssertionError, IndexError, KeyError) as error:
            self.server.errors.append(repr(error))
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


def main():
    binary = str(Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory() as cwd, Proxy(('127.0.0.1', 0), Tunnel) as proxy:
        cert, key = Path(cwd) / 'cert.pem', Path(cwd) / 'key.pem'
        subprocess.run([
            'openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-days', '1',
            '-keyout', str(key), '-out', str(cert), '-subj', '/CN=jwxt.shmtu.edu.cn',
            '-addext', 'subjectAltName=DNS:jwxt.shmtu.edu.cn',
            '-addext', 'basicConstraints=critical,CA:FALSE',
        ], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        proxy.tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        proxy.tls.load_cert_chain(cert, key)
        proxy.steps, proxy.seen, proxy.errors = collections.deque(), [], []
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
            deadline = time.monotonic() + 15
            while text.encode() not in output[start:]:
                remaining = deadline - time.monotonic()
                assert remaining > 0, output[start:].decode(errors='replace')
                if select.select([master], [], [], remaining)[0]:
                    output.extend(os.read(master, 65536))

        def command(text, result):
            mark = len(output)
            os.write(master, (text + '\r').encode())
            wait(result, mark)
            result_end = output.index(result.encode(), mark) + len(result.encode())
            wait('course-election>', result_end)

        def case(label, cmd, steps, result):
            assert not proxy.steps
            start = len(proxy.seen)
            proxy.steps.extend(steps)
            command(cmd, result)
            assert not proxy.errors, proxy.errors
            assert not proxy.steps, list(proxy.steps)
            assert proxy.seen[start:] == [(m, t) for m, t, _ in steps]
            print('PASS:', label)

        try:
            wait('course-election>')
            command('profile 3112', 'profile=3112')
            command('target 252279', 'target=252279')
            case('ordinary failure reuses HTML token despite different Date', 'fire 3 0', [
                ('GET', '', page(A)), ('POST', A, '选课失败:人数已满'),
                ('POST', A, '选课失败:人数已满'), ('POST', A, '选课成功'),
            ], '[3] 选课成功')
            case('stale token refreshes on next allowed attempt', 'fire 3 0', [
                ('GET', '', page(A)), ('POST', A, STALE),
                ('GET', '', page(B)), ('POST', B, '选课成功'),
            ], '[2] 选课成功')
            case('exhausted budget does not refresh or POST again', 'fire 1 0', [
                ('GET', '', page(A)), ('POST', A, STALE),
            ], '达到最大尝试次数')
            case('new fire obtains its own token', 'fire 1 0', [
                ('GET', '', page(B)), ('POST', B, '选课成功'),
            ], '[1] 选课成功')
            case('missing token prevents POST', 'fire 1 0', [
                ('GET', '', '<html>登录失效</html>'),
            ], '达到最大尝试次数')
            case('drop remains one POST with undefined', 'drop 1 0', [
                ('POST', 'undefined', '退课成功'),
            ], '[1] 退课成功')
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
