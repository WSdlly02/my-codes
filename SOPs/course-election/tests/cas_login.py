"""Exercise CLI login, including CAS MFA, against a local TLS proxy after cargo build --bins."""
import json
from http.server import BaseHTTPRequestHandler
from pathlib import Path
import subprocess
import tempfile
from urllib.parse import parse_qs, urlsplit

import fixture
from daemon_path import ROOT, until

JWXT = "jwxt.shmtu.edu.cn"
CAS = "sso.shmtu.edu.cn"
OCR = "api.deepseek.com"
HOME = "/shmtu/home.action"
SERVICE = f"http://{JWXT}{HOME};jsessionid=fixture"
PAGES = json.loads((ROOT / "tests/fixtures/cas-mfa.json").read_text())["pages"]


class Reply(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def reply(self, body="", status=200, headers=()):
        self.send_response(status)
        self.send_header("Content-Length", str(len(body.encode())))
        for key, value in headers:
            self.send_header(key, value)
        self.end_headers()
        self.wfile.write(body.encode())

    def do_GET(self):
        host = self.headers["Host"]
        path = urlsplit(self.path).path
        if (host, path) == (JWXT, HOME):
            self.reply(status=302, headers=[("Location", f"https://{CAS}/cas/login?service={SERVICE}")])
        elif (host, path) == (CAS, "/cas/login"):
            self.reply(PAGES["password"])
        elif (host, path) == (CAS, "/cas/captcha"):
            self.reply(json.dumps(dict(image="data:image/png;base64,iVBORw0KGgo=", token="challenge", expiresAt=2**62)))
        elif host == JWXT and self.path == f"{HOME};jsessionid=fixture?ticket=test":
            self.reply(headers=[("Set-Cookie", "JSESSIONID=fixture; Path=/shmtu; Secure")])
        elif host == JWXT and path == "/shmtu/stdElectCourse.action":
            assert "JSESSIONID=fixture" in self.headers["Cookie"]
            self.reply()
        else:
            self.send_error(404)

    def do_POST(self):
        body = self.rfile.read(int(self.headers["Content-Length"]))
        if self.headers["Host"] == OCR:
            assert self.path == "/v1/chat/completions"
            assert self.headers["Authorization"] == "Bearer test-key"
            assert self.headers.get("Cookie") is None
            self.reply(json.dumps({"choices": [{"finish_reason": "stop", "message": {"content": '{"e":"4*3"}'}}]}))
            return

        assert self.headers["Host"] == CAS and self.path == "/cas/login"
        form = parse_qs(body.decode())
        step = form["execution"][0]
        self.server.steps.append(step)
        if step == "fixture-password":
            assert [form[k][0] for k in ("username", "password", "validateCode", "captchaToken")] == ["user", "pass", "12", "challenge"]
            self.reply(PAGES["mfa_choice"])
        elif step == "fixture-choice":
            assert form["mfaProvider"] == ["mfa-gauth"]
            self.reply(PAGES["totp"])
        elif step == "fixture-totp":
            assert form["accountId"] == ["424242"] and len(form["token"][0]) == 6
            self.reply(PAGES["trust_device"])
        elif step == "fixture-trust":
            assert form["_eventId"] == ["skip"] and "trustDevice" not in form
            self.reply(status=302, headers=[("Location", f"{SERVICE}?ticket=test")])
        else:
            self.send_error(400)


if __name__ == "__main__":
    with tempfile.TemporaryDirectory() as directory:
        proxy, env = fixture.start_proxy(directory, Reply, (JWXT, CAS, OCR))
        proxy.steps = []
        env.pop("DEEPSEEK_API_KEY", None)
        env.pop("TOTP_BASE32", None)
        env["TMPDIR"] = directory
        client_dir = Path(directory) / "client"
        client_dir.mkdir()
        credentials = client_dir / ".env"
        values = "USERNAME=user\nPASSWORD='pass'\nTOTP_BASE32=GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ\nDEEPSEEK_API_KEY=test-key\n"
        credentials.write_text(values)
        daemon = subprocess.Popen([ROOT / "target/debug/course-electiond", "--data-dir", directory], env=env)
        try:
            until((Path(directory) / "cache/runtime/daemon.sock").exists)
            login = [ROOT / "target/debug/course-election", "--data-dir", directory, "login"]
            options = dict(cwd=client_dir, stdin=subprocess.DEVNULL, timeout=20)
            subprocess.run(login, env=env, check=True, **options)
            # Read the selected file again, honoring the calling CLI's env overrides.
            other = client_dir / "other.env"
            other.write_text(values.replace("test-key", "unused").replace("GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ", "invalid"))
            subprocess.run(login + ["--credentials", "other.env"],
                           env=dict(env, DEEPSEEK_API_KEY="test-key", TOTP_BASE32="GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"),
                           check=True, **options)
            assert proxy.steps == ["fixture-password", "fixture-choice", "fixture-totp", "fixture-trust"] * 2
            credentials.write_text("USERNAME=user\n")
            missing = subprocess.run(login, env=env, capture_output=True, **options)
            assert missing.returncode and b"PASSWORD" in missing.stderr
            assert not proxy.errors, proxy.errors
        finally:
            daemon.terminate()
            daemon.wait(timeout=5)
            proxy.shutdown()
            proxy.server_close()
