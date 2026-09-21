"""Exercise arm against a stalled local CONNECT proxy; never reaches JWXT."""
import datetime
import os
from pathlib import Path
import pty
import select
import socketserver
import subprocess
import sys
import tempfile
import threading
import time


class Stall(socketserver.BaseRequestHandler):
    def handle(self):
        self.request.recv(4096)
        self.server.requests += 1
        self.server.stopped.wait(20)


class Proxy(socketserver.ThreadingTCPServer):
    daemon_threads = True


def main():
    binary = str(Path(sys.argv[1]).resolve())
    with Proxy(('127.0.0.1', 0), Stall) as proxy, tempfile.TemporaryDirectory() as cwd:
        proxy.requests = 0
        proxy.stopped = threading.Event()
        threading.Thread(target=proxy.serve_forever, daemon=True).start()
        env = os.environ.copy()
        endpoint = f'http://127.0.0.1:{proxy.server_address[1]}'
        for key in ('HTTP_PROXY', 'HTTPS_PROXY', 'ALL_PROXY', 'http_proxy', 'https_proxy', 'all_proxy'):
            env[key] = endpoint
        env['NO_PROXY'] = env['no_proxy'] = ''
        master, slave = pty.openpty()
        proc = subprocess.Popen([binary], cwd=cwd, env=env, stdin=slave, stdout=slave, stderr=slave)
        os.close(slave)
        output = bytearray()

        def wait(text, start=0, timeout=5):
            deadline = time.monotonic() + timeout
            while text.encode() not in output[start:]:
                remaining = deadline - time.monotonic()
                assert remaining > 0, f'timeout waiting for {text}: {output[start:]!r}'
                if select.select([master], [], [], remaining)[0]:
                    output.extend(os.read(master, 65536))

        def send(text):
            start = len(output)
            os.write(master, (text + '\r').encode())
            return start

        try:
            wait('course-election>')
            wait('profile=1', send('profile 1'))
            wait('target=1', send('target 1'))
            mark = send('arm')
            wait('arm>', mark)
            time.sleep(.15)
            assert proxy.requests > 0, 'prewarm must actually be in flight'
            started = time.monotonic()
            wait('已取消', send('cancel'), timeout=1)
            assert time.monotonic() - started < 1
            print('PASS: cancel interrupts in-flight prewarm')

            time.sleep(.1)
            before = proxy.requests
            started = time.monotonic()
            mark = send('arm')
            wait('预热失败', mark, timeout=3)
            assert time.monotonic() - started < 3
            assert proxy.requests - before == 2, 'two endpoints, no retries'
            wait('已取消', send('cancel'))
            print('PASS: prewarm timeout is bounded and not retried')

            target = datetime.datetime.now(datetime.timezone.utc) + datetime.timedelta(seconds=1)
            started = time.monotonic()
            mark = send('arm ' + target.isoformat())
            wait('定时触发偏差', mark, timeout=1.8)
            assert time.monotonic() - started < 1.8
            print('PASS: scheduled trigger does not await two-second prewarm')
        finally:
            proc.terminate()
            proc.wait(timeout=5)
            os.close(master)
            proxy.stopped.set()
            proxy.shutdown()


if __name__ == '__main__':
    main()
