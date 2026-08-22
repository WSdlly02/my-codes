#!/usr/bin/env python3
"""Log in to the running course-election REPL using USERNAME/PASSWORD from .env.

The password is read inside this script and sent straight to the REPL's hidden
password prompt (rpassword over PTY). It is never printed, never written to disk.

Usage:
  python3 repl_login.py [--env PATH] [--socket PATH]
"""
import argparse
import os
import re
import select
import socket
import sys
import time


def parse_args():
    ap = argparse.ArgumentParser(description=__doc__)
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_dir = os.path.dirname(script_dir)
    ap.add_argument('--env', default=os.path.join(project_dir, '.env'))
    ap.add_argument('--socket', default='/tmp/course-repl.sock')
    ap.add_argument('--timeout', type=float, default=180.0)
    return ap.parse_args()


def load_env(path):
    values = {}
    with open(path, encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#') or '=' not in line:
                continue
            line = re.sub(r'^export\s+', '', line)
            key, _, val = line.partition('=')
            val = val.strip()
            if len(val) >= 2 and val[0] == val[-1] and val[0] in ('"', "'"):
                val = val[1:-1]
            values[key.strip()] = val
    return values


def main():
    args = parse_args()
    env = load_env(args.env)
    username = env.get('USERNAME', '')
    password = env.get('PASSWORD', '')
    if not username or not password:
        print(f'错误：{args.env} 中缺少 USERNAME 或 PASSWORD', file=sys.stderr)
        return 1

    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(args.socket)
    s.setblocking(False)
    s.sendall(('login ' + username + '\n').encode())

    prompt_marker = '密码'.encode('utf-8')
    done_marker = 'course-election> '.encode('utf-8')
    out = b''
    password_sent = False
    start = time.time()
    while time.time() - start < args.timeout:
        if not password_sent and prompt_marker in out:
            s.sendall(password.encode() + b'\n')
            password_sent = True
        r, _, _ = select.select([s], [], [], 0.2)
        if r:
            try:
                chunk = s.recv(65536)
            except OSError:
                break
            if not chunk:
                break
            out += chunk
            if password_sent and done_marker in out:
                break
    s.close()
    sys.stdout.buffer.write(out)
    return 0


if __name__ == '__main__':
    sys.exit(main())
