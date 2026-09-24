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
        elif "queryStdCount" in url.path and server.login_lost:
            self.send_response(302)
            self.send_header("Location", "https://ng.shmtu.edu.cn/wengine-auth/login?id=170&path=/")
            self.send_header("Content-Length", "0")
            self.end_headers()
            return
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
            data = server.post_reply if status == 200 else "server error"
        elif url.path == "/shmtu/stdElectCourse.action":
            server.prewarms += 1
            data = ""
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
        proxy.log, proxy.post_times, proxy.prewarms = [], [], 0
        proxy.full, proxy.delay, proxy.token = True, 0, 20260923100000
        proxy.post_status, proxy.post_reply = 200, "选课成功"
        proxy.login_lost = False
        cwd = Path(directory)
        socket_path = cwd / "cache/runtime/daemon.sock"
        daemon_cmd = [str(ROOT / "target/debug/course-electiond"), "--data-dir", directory, "--poll", "1s"]
        with (cwd / "daemon.log").open("w+") as log:
            daemon = subprocess.Popen(daemon_cmd, env=env, stdout=log, stderr=log)
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

                def maintain(op, **fields):
                    return rpc({"command": "maintenance", "op": op, **fields})

                def watch(lesson):
                    spec = {"lesson": str(lesson), "kind": "watch", "timeout_ms": 60000, "dry_run": False}
                    return rpc({"command": "add", "spec": spec, "wait": False})["id"]

                def fire(lesson, select=True, attempts=1, at_ms=None):
                    spec = {"lesson": str(lesson), "kind": "fire", "select": select, "at_ms": at_ms, "attempts": attempts, "interval_ms": 300}
                    return rpc({"command": "add", "spec": spec, "wait": True})["progress"]

                def running(i):
                    return i in [j["id"] for j in rpc({"command": "jobs"})]

                def ended(i, phase):
                    return any(f"意图 {i} 结束：{phase}" in e["message"] for e in rpc({"command": "logs"})["events"])

                def posts():
                    return sum(row[0] == "POST" for row in proxy.log)

                def reads():
                    return sum("queryStdCount" in row[1] for row in proxy.log)

                second = subprocess.run(daemon_cmd, capture_output=True, timeout=5)
                assert second.returncode != 0 and rpc({"command": "status"})["profile"] is None
                maintain("use_profile", id="3112")

                # Two watches share one poller and never POST while the course is full.
                a, b = watch(101), watch(102)
                until(lambda: reads() >= 2)
                before = reads()
                time.sleep(3.2)
                assert reads() - before <= 4, "one shared read per poll, not one per watch"
                assert posts() == 0

                # Cancel stops waiting; a handed-off POST still completes and is recorded.
                rpc({"command": "cancel", "id": a})
                until(lambda: not running(a))
                assert ended(a, "已取消")
                proxy.full, proxy.delay = False, 1.5
                until(lambda: posts() == 1)
                t = time.monotonic()
                assert rpc({"command": "cancel", "id": b})["progress"]["phase"] == "submitting"
                assert time.monotonic() - t < .5
                until(lambda: ended(b, "成功"))

                # Reads keep going while a POST is pending.
                c = watch(103)
                until(lambda: posts() == 2)
                pending = reads()
                until(lambda: reads() > pending)
                until(lambda: ended(c, "成功"))

                # Intents end once their context is replaced.
                proxy.full, proxy.delay = True, 0
                d = watch(106)
                maintain("use_profile", id="3113")
                until(lambda: ended(d, "已取消"))
                assert rpc({"command": "status"})["profile"] == "3113"

                assert fire(104, select=False)["phase"] == "succeeded"

                proxy.post_reply, count = "选课失败:未开放", posts()
                end = fire(108, attempts=3)
                assert (end["phase"], end["attempts"], posts()) == ("failed", 3, count + 3)

                proxy.post_status, count = 500, posts()
                assert fire(105, attempts=3)["phase"] == "unknown" and posts() == count + 1

                # A timed fire warms up early and fires on its own timer.
                proxy.post_status, proxy.post_reply, count = 200, "选课成功", posts()
                warmed = proxy.prewarms
                at_ms = int(time.time() * 1000) + 1500
                assert fire(107, at_ms=at_ms)["phase"] == "succeeded"
                late_ms = proxy.post_times[-1] * 1000 - at_ms
                assert posts() == count + 1 and proxy.prewarms == warmed + 1 and 0 <= late_ms < 200, late_ms
                print(f"timed fire sent {late_ms:.0f}ms after --at")

                cli = [str(ROOT / "target/debug/course-election"), "--data-dir", directory]
                result = subprocess.run([*cli, "--json", "status"], env=env, capture_output=True, timeout=5)
                assert result.returncode == 0, result.stderr
                assert json.loads(result.stdout)["data"]["profile"] == "3113"
                for args in (["daemon", "stop"], ["arm"], ["jobs"], ["prepare"], ["job", "pause", "1"], ["status", "--force"]):
                    assert subprocess.run([*cli, *args], capture_output=True).returncode != 0

                # A read redirected to the gateway login page is reported once, with the time.
                e = watch(109)
                proxy.login_lost = True
                until(lambda: rpc({"command": "status"})["login_lost_at_ms"] is not None)
                time.sleep(2.2)
                lost = [m["message"] for m in rpc({"command": "logs"})["events"] if "登录已失效" in m["message"]]
                assert len(lost) == 1, lost
                proxy.login_lost = False

                # Logout cancels everything and clears the login record.
                maintain("logout")
                until(lambda: ended(e, "已取消") and rpc({"command": "jobs"}) == [])
                assert rpc({"command": "status"})["login_lost_at_ms"] is None

                assert not proxy.errors, proxy.errors
                daemon.terminate()
                assert daemon.wait(timeout=10) == 0
                assert not socket_path.exists()
                print("PASS: lock, shared poller, cancel boundary, reads during POST, profile switch, drop, retries, unknown, timed fire, CLI, login loss, logout, cleanup")
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
