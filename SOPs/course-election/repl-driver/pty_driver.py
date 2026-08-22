#!/usr/bin/env python3
"""Long-lived PTY wrapper: run an interactive REPL program under a pseudo-terminal
and expose it over a Unix socket. Each socket client can send input and receive output.

Usage:
  python3 pty_driver.py --socket /tmp/course-repl.sock --cmd ./course-election [--cwd /path]

The driver keeps the child alive until it exits. Send 'quit' to the REPL to stop,
or kill the driver process (it cleans up the socket and terminates the child).
"""
import argparse
import os
import pty
import select
import signal
import socket


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--socket', required=True)
    ap.add_argument('--cwd', default=None, help='working directory for the child')
    ap.add_argument('--cmd', nargs=argparse.REMAINDER, required=True)
    args = ap.parse_args()

    sock_path = args.socket
    if os.path.exists(sock_path):
        os.unlink(sock_path)

    pid, master_fd = pty.fork()
    if pid == 0:
        if args.cwd:
            os.chdir(args.cwd)
        try:
            os.execvp(args.cmd[0], args.cmd)
        except Exception:
            os._exit(127)

    srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    srv.bind(sock_path)
    srv.listen(16)
    clients = set()
    rlist = [master_fd, srv]
    alive = True

    def request_stop(_sig=None, _frm=None):
        nonlocal alive
        alive = False

    signal.signal(signal.SIGTERM, request_stop)
    signal.signal(signal.SIGINT, request_stop)

    try:
        while alive:
            readable, _, _ = select.select(rlist, [], [], 0.5)
            for fd in readable:
                if fd == srv:
                    conn, _ = srv.accept()
                    conn.setblocking(False)
                    clients.add(conn)
                    rlist.append(conn)
                elif fd == master_fd:
                    try:
                        data = os.read(master_fd, 65536)
                    except OSError:
                        data = b''
                    if not data:
                        for c in clients:
                            try:
                                c.sendall(b'\n[pty-driver] child exited\n')
                            except OSError:
                                pass
                        alive = False
                        break
                    for c in list(clients):
                        try:
                            c.sendall(data)
                        except OSError:
                            clients.discard(c)
                            if c in rlist:
                                rlist.remove(c)
                            c.close()
                else:
                    try:
                        data = fd.recv(65536)
                    except OSError:
                        data = b''
                    if not data:
                        clients.discard(fd)
                        if fd in rlist:
                            rlist.remove(fd)
                        fd.close()
                    else:
                        try:
                            os.write(master_fd, data)
                        except OSError:
                            pass
    finally:
        try:
            os.kill(pid, signal.SIGTERM)
        except OSError:
            pass
        try:
            os.unlink(sock_path)
        except OSError:
            pass


if __name__ == '__main__':
    main()
