#!/usr/bin/env python3
"""CLI/daemon integration using local TLS CONNECT only. No real course writes.

Run after cargo build --bins: python3 tests/daemon_path.py
"""
import http.client
import http.server
import json
from pathlib import Path
import socket
import subprocess
import tempfile
import time
import urllib.parse
import fixture

ROOT = Path(__file__).resolve().parents[1]


class Reply(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_GET(self):
        self.reply()

    def do_POST(self):
        self.reply()

    def reply(self):
        server = self.server
        url = urllib.parse.urlsplit(self.path)
        query = urllib.parse.parse_qs(url.query)
        body = self.rfile.read(int(self.headers.get("Content-Length", 0))).decode()
        server.log.append((self.command, url.path, query, body))
        status = 200
        if "defaultPage" in url.path:
            server.token += 1
            data = fixture.page(str(server.token)) + '<script>var electedIds={};electedIds["l105"]=true;</script>'
        elif "queryStdCount" in url.path:
            count = 1 if server.full else 0
            data = "window.lessonId2Counts={" + ",".join(
                f"'{i}':{{sc:{count},lc:1,wc:0}}" for i in range(101, 111)) + "};"
        elif "batchOperator" in url.path:
            server.post_times.append(time.time())
            operator = urllib.parse.parse_qs(body)["operator0"][0]
            expected = "undefined" if ":false" in operator else str(server.token)
            if query.get("elecSessionTime") != [expected]:
                server.errors.append("wrong token")
            time.sleep(server.delay)
            status = server.post_status
            data = "选课成功" if status == 200 else "server error"
        elif url.path == "/shmtu/stdElectCourse.action":
            data = ""  # arm's connection prewarm
        else:
            server.errors.append(url.path)
            status, data = 500, "unexpected"
        raw = data.encode()
        self.send_response(status)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)


class UnixConnection(http.client.HTTPConnection):
    def __init__(self, path):
        super().__init__("daemon", timeout=15)
        self.path = path

    def connect(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.connect(self.path)


def until(predicate, timeout=12):
    end = time.monotonic() + timeout
    while not predicate():
        assert time.monotonic() < end, "condition timed out"
        time.sleep(.04)


def main():
    with tempfile.TemporaryDirectory() as directory:
        proxy, env = fixture.start_proxy(directory, Reply)
        proxy.log, proxy.post_times = [], []
        proxy.full, proxy.delay, proxy.post_status, proxy.token = True, 0, 200, 20260923100000
        cwd = Path(directory)
        socket_path = cwd / "cache/runtime/daemon.sock"
        with (cwd / "daemon.log").open("w+") as log:
            daemon = subprocess.Popen([str(ROOT / "target/debug/course-electiond"), "--data-dir", directory], env=env, stdout=log, stderr=log)
            try:
                until(socket_path.exists)

                def rpc(command, ok=True):
                    connection = UnixConnection(str(socket_path))
                    connection.request("POST", "/v1/command", json.dumps(command), {"Content-Type": "application/json"})
                    response = connection.getresponse()
                    text = response.read().decode()
                    connection.close()
                    assert (response.status == 200) == ok, text
                    return json.loads(text).get("data") if ok else response.status

                def profile(id, force=False, ok=True):
                    return rpc({"command": "maintenance", "op": "profile", "id": id, "force": force}, ok)

                def add(lesson, mode="watch"):
                    return rpc({"command": "add", "spec": {"lesson": str(lesson), "mode": mode, "interval_ms": 1000, "attempts": 0 if mode == "watch" else 1, "timeout_ms": 60000, "at_ms": None, "dry_run": False}})["id"]

                def job(i):
                    return rpc({"command": "job", "id": i})

                def posts():
                    return sum(row[0] == "POST" for row in proxy.log)

                assert rpc({"command": "status"})["profile"] is None
                second = subprocess.run([str(ROOT / "target/debug/course-electiond"), "--data-dir", directory], capture_output=True, timeout=5)
                assert second.returncode != 0 and rpc({"command": "status"})["profile"] is None
                profile("3112")
                a, b = add(101), add(102)
                until(lambda: sum("queryStdCount" in row[1] for row in proxy.log) >= 2)
                assert posts() == 0
                pages = proxy.token
                profile("3113", ok=False)
                assert proxy.token == pages
                rpc({"command": "cancel", "id": a})
                proxy.full, proxy.delay = False, 1.5
                until(lambda: posts() == 1)
                t = time.monotonic()
                assert rpc({"command": "status"})["inflight"] == b
                assert time.monotonic() - t < .5
                rpc({"command": "cancel", "id": b})
                until(lambda: job(b)["phase"] == "succeeded")
                assert job(a)["phase"] == "cancelled"
                assert posts() == 1 and proxy.token == pages
                # Write is serial, read-side continues while POST is pending.
                c = add(103)
                until(lambda: posts() == 2)
                reads = sum("queryStdCount" in row[1] for row in proxy.log)
                until(lambda: sum("queryStdCount" in row[1] for row in proxy.log) > reads)
                profile("3113", force=True)
                assert job(c)["phase"] == "succeeded"
                assert rpc({"command": "status"})["profile"] == "3113"
                proxy.delay = 0
                d = add(104, "drop")
                until(lambda: job(d)["phase"] == "succeeded")
                proxy.post_status = 500
                e = add(105)
                until(lambda: job(e)["phase"] == "unknown")
                count = posts()
                time.sleep(1.2)
                assert posts() == count
                rpc({"command": "maintenance", "op": "reconcile", "id": e})
                assert job(e)["phase"] == "succeeded" and posts() == count
                proxy.full = True
                pending = add(106)
                profile("3112", force=True)
                assert job(pending)["phase"] == "cancelled"
                assert rpc({"command": "status"})["profile"] == "3112"
                # Arm fires on its own timer, not a polling tick.
                proxy.post_status, count = 200, posts()
                at_ms = int(time.time() * 1000) + 1500
                arm = rpc({"command": "add", "spec": {"lesson": "107", "mode": "arm", "interval_ms": 500, "attempts": 2, "timeout_ms": 0, "at_ms": at_ms, "dry_run": False}})["id"]
                until(lambda: job(arm)["phase"] == "succeeded")
                late_ms = proxy.post_times[-1] * 1000 - at_ms
                assert posts() == count + 1 and 0 <= late_ms < 200, late_ms
                print(f"arm fired {late_ms:.0f}ms after --at")
                result = subprocess.run([str(ROOT / "target/debug/course-election"), "--data-dir", directory, "--json", "status"], env=env, capture_output=True, timeout=5)
                assert result.returncode == 0, result.stderr
                assert json.loads(result.stdout)["data"]["profile"] == "3112"
                for args in (["daemon", "stop"], ["status", "--force"]):
                    assert subprocess.run([str(ROOT / "target/debug/course-election"), *args], capture_output=True).returncode != 0
                assert not proxy.errors, proxy.errors
                daemon.terminate()
                assert daemon.wait(timeout=10) == 0
                assert not socket_path.exists()
                print("PASS: lock, shared reads/token, cancellation, force switch, drop, unknown, arm timing, CLI, cleanup")
            finally:
                if daemon.poll() is None:
                    daemon.kill()
                    daemon.wait()
                if daemon.returncode != 0:
                    log.seek(0)
                    print(log.read())
                proxy.shutdown()


if __name__ == "__main__":
    main()
