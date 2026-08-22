#!/usr/bin/env python3
"""Send payload (stdin) to a REPL running under pty_driver.py, print responses.

Usage:
  echo 'status' | pty_client.py /tmp/course-repl.sock
  echo 'find 海事法' | pty_client.py /tmp/course-repl.sock --wait 'course-election> '
  echo 'fire 20 500' | pty_client.py /tmp/course-repl.sock --wait 'course-election> '

Options:
  --wait MARKER   stop as soon as MARKER appears in the output (use the REPL prompt
                  to capture the full response of long commands like login/refresh).
  --idle SECS     stop after this many seconds of silence (default 1.2; used when
                  --wait is not given, e.g. for 'arm' which has no prompt while waiting).
  --max SECS      hard cap on total listening time (default 120).
"""
import argparse
import select
import socket
import sys
import time


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('socket')
    ap.add_argument('--wait', default=None)
    ap.add_argument('--idle', type=float, default=1.2)
    ap.add_argument('--max', type=float, default=120.0)
    args = ap.parse_args()

    payload = sys.stdin.buffer.read()
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(args.socket)
    s.setblocking(False)
    if payload:
        s.sendall(payload)

    out = b''
    start = time.time()
    deadline = start + args.idle
    while time.time() - start < args.max:
        now = time.time()
        if args.wait is None and now >= deadline:
            break
        r, _, _ = select.select([s], [], [], 0.2)
        if r:
            try:
                chunk = s.recv(65536)
            except OSError:
                break
            if not chunk:
                break
            out += chunk
            if args.wait and args.wait.encode() in out:
                break
            deadline = now + args.idle
    s.close()
    sys.stdout.buffer.write(out)


if __name__ == '__main__':
    main()
