#!/usr/bin/env python3
"""Minimal HTTP server that dumps request headers and body to stdout."""
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class DumpHandler(BaseHTTPRequestHandler):
    def _handle(self):
        length = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(length) if length else b""

        print(f"\n{'=' * 50}")
        print(f">>> {self.command} {self.path}")
        print(f">>> from {self.client_address[0]}:{self.client_address[1]}")
        print("--- Headers ---")
        for key, value in self.headers.items():
            print(f"{key}: {value}")
        print("--- Body ---")
        if body:
            try:
                print(body.decode("utf-8"))
            except UnicodeDecodeError:
                print(f"<binary, {len(body)} bytes> {body!r}")
        else:
            print("<empty>")

        resp = b'{"status":"ok"}'
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(resp)))
        self.end_headers()
        self.wfile.write(resp)

    do_GET = do_POST = do_PUT = do_PATCH = do_DELETE = _handle

    def log_message(self, fmt, *args):  # silence default access log
        pass


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8000
    server = ThreadingHTTPServer(("0.0.0.0", port), DumpHandler)
    print(f"Listening on http://0.0.0.0:{port}  (Ctrl+C to stop)")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
