#!/usr/bin/env python3
"""通用 REPL 操作工具：原生终端 + agent 通道。

一个进程做三件事：
  1. 前台运行任意交互式程序（run <程序>），用户获得原生终端体验（无 Zellij/tmux 包裹）；
  2. 把程序输出追加到 --log（完整历史，cat 一次 = 启动以来的全部输出）；
  3. 监听 --in-sock，agent/人类可发命令（模拟键盘输入写入 PTY）。

用法：
  # 前台运行（原生终端体验）
  python3 repl-pty.py run './course-election' --log repl.log --in-sock agent.sock

  # 发命令（agent 或人类，一条命令一个短进程）
  python3 repl-pty.py write 'find 海事法' --in-sock agent.sock

  # 读输出：从最近一次 write 前累计读取 | --all 全量历史 | --wait 等待新输出
  python3 repl-pty.py read --log repl.log
  python3 repl-pty.py read --all --log repl.log
  python3 repl-pty.py read --wait 5 --log repl.log

通用：run 的程序可以是 course-election、python3 -i、node、bash 或其他 REPL。
"""

import argparse
import atexit
import fcntl
import os
import pty
import re
import select
import signal
import socket
import stat
import struct
import sys
import termios
import time

DEFAULT_SOCK = "/tmp/repl-pty.sock"
DEFAULT_LOG = "repl.log"


# ---------------------------------------------------------------- run

def setup_raw_terminal():
    """把用户终端设为 raw mode：按键立即转发、不 echo（echo 由子进程 PTY 负责）。"""
    fd = sys.stdin.fileno()
    if not os.isatty(fd):
        return None  # 非交互（如管道），跳过
    old = termios.tcgetattr(fd)
    tty_raw = termios.tcgetattr(fd)
    # tcgetattr 返回 [iflag, oflag, cflag, lflag, ispeed, ospeed, cc]
    tty_raw[3] &= ~(termios.ECHO | termios.ICANON | termios.ISIG | termios.IEXTEN)  # lflag
    tty_raw[0] &= ~(termios.IXON | termios.ICRNL)  # iflag
    termios.tcsetattr(fd, termios.TCSANOW, tty_raw)
    return old


def restore_terminal(old):
    if old is not None:
        try:
            termios.tcsetattr(sys.stdin.fileno(), termios.TCSANOW, old)
        except OSError:
            pass


def remove_file(path):
    try:
        os.unlink(path)
    except FileNotFoundError:
        pass


def make_socket(path):
    try:
        mode = os.lstat(path).st_mode
    except FileNotFoundError:
        pass
    else:
        if not stat.S_ISSOCK(mode):
            raise RuntimeError(f"输入 socket 路径已被非 socket 文件占用: {path}")
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as probe:
            try:
                probe.connect(path)
            except ConnectionRefusedError:
                remove_file(path)
            else:
                raise RuntimeError(f"已有 REPL 正在使用输入 socket: {path}")
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(path)
    os.chmod(path, 0o600)
    server.listen(8)
    return server


def open_log(path):
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o600)
    return os.fdopen(fd, "ab", buffering=0)


def sync_window_size(master):
    try:
        if not os.isatty(sys.stdin.fileno()):
            return
        rows, cols = termios.tcgetwinsize(sys.stdin.fileno())
    except Exception:
        return
    fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


def write_all(fd, data):
    while data:
        data = data[os.write(fd, data):]


def drop_input_echo(data, active):
    r"""Drop terminal redraws through the submitted line's first newline.

    >>> drop_input_echo(b"prompt> f", True)
    (b'', True)
    >>> drop_input_echo(b"prompt> find\r\nresult\r\n", True)
    (b'result\r\n', False)
    >>> drop_input_echo(b"result\r\n", False)
    (b'result\r\n', False)
    """
    if not active:
        return data, False
    _echo, newline, output = data.partition(b"\n")
    return (output, False) if newline else (b"", True)


ANSI_CSI = re.compile(rb"\x1b\[[0-9;?<>]*[a-zA-Z]")  # 光标移动/清屏/颜色等 CSI 序列
ANSI_OSC = re.compile(rb"\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)")  # 标题等 OSC 序列


def strip_ansi(data):
    """去掉 ANSI 控制序列，只保留可见文本（用于写入日志）。"""
    return ANSI_OSC.sub(b"", ANSI_CSI.sub(b"", data))


def cmd_run(args):
    old_term = setup_raw_terminal()
    atexit.register(restore_terminal, old_term)

    server = make_socket(args.in_sock)
    atexit.register(remove_file, args.in_sock)

    log = open_log(args.log)
    atexit.register(remove_file, args.log)
    atexit.register(remove_file, args.log + ".baseline")
    atexit.register(log.close)

    pid, master = pty.fork()
    if pid == 0:  # 子进程：运行目标命令
        os.execvpe("/bin/sh", ["/bin/sh", "-c", args.cmd], os.environ)

    sync_window_size(master)
    signal.signal(signal.SIGWINCH, lambda *_: sync_window_size(master))

    clients = []
    started_clients = set()
    dropping_input_echo = False
    try:
        while True:
            rlist = [master, sys.stdin, server] + clients
            readable, _, _ = select.select(rlist, [], [], 1.0)
            for fd in readable:
                if fd == master:
                    try:
                        data = os.read(master, 65536)
                    except OSError:
                        data = b""
                    if not data:  # 子进程退出
                        return
                    write_all(sys.stdout.fileno(), data)
                    data, dropping_input_echo = drop_input_echo(data, dropping_input_echo)
                    log.write(strip_ansi(data))
                elif fd is sys.stdin:
                    data = os.read(sys.stdin.fileno(), 65536)
                    if not data:
                        return  # 用户终端 EOF
                    write_all(master, data)
                elif fd is server:
                    conn, _ = server.accept()
                    conn.setblocking(False)
                    clients.append(conn)
                else:
                    try:
                        data = fd.recv(65536)
                    except (BlockingIOError, ConnectionResetError):
                        data = b""
                    if not data:
                        clients.remove(fd)
                        started_clients.discard(fd)
                        fd.close()
                    else:
                        if fd not in started_clients:
                            started_clients.add(fd)
                            dropping_input_echo = True
                        write_all(master, data)
    finally:
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        for conn in clients:
            conn.close()
        server.close()


# ---------------------------------------------------------------- write

def cmd_write(args):
    baseline_file = args.log + ".baseline"
    fd = os.open(baseline_file, os.O_RDWR | os.O_CREAT, 0o600)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        baseline = os.path.getsize(args.log) if os.path.exists(args.log) else 0
        os.ftruncate(fd, 0)
        os.write(fd, str(baseline).encode())
        os.fsync(fd)
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
            sock.connect(args.in_sock)
            data = args.command_arg.encode() + (b"\n" if args.lf else b"\r")
            sock.sendall(data)
    finally:
        os.close(fd)


# ---------------------------------------------------------------- read

def cmd_read(args):
    baseline_file = args.log + ".baseline"

    if args.all:
        if not os.path.exists(args.log):
            return
        with open(args.log, "rb") as f:
            sys.stdout.buffer.write(f.read())
        sys.stdout.buffer.flush()
        return

    if not os.path.exists(args.log):
        return  # 尚未启动/无输出，空结果

    if not os.path.exists(baseline_file):
        return  # 尚未 write，没有观察窗口

    fd = os.open(baseline_file, os.O_RDONLY)
    try:
        fcntl.flock(fd, fcntl.LOCK_SH)
        offset = int(os.read(fd, 65536) or 0)
        size = os.path.getsize(args.log)
        if offset > size:  # 日志被轮转/截断，从头
            offset = 0
        if args.wait:
            deadline = time.monotonic() + args.wait
            while size <= offset and time.monotonic() < deadline:
                time.sleep(0.1)
                size = os.path.getsize(args.log)
        with open(args.log, "rb") as f:
            f.seek(offset)
            data = f.read()
        sys.stdout.buffer.write(data)
        sys.stdout.buffer.flush()
    finally:
        os.close(fd)


# ---------------------------------------------------------------- main

def build_parser():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)

    run = commands.add_parser("run")
    run.add_argument("cmd", help="要运行的交互式程序")

    write = commands.add_parser("write")
    write.add_argument("command_arg", help="要提交的单行命令")
    write.add_argument("--lf", action="store_true", help="使用 \\n 代替 \\r 作为回车")

    read = commands.add_parser("read")
    read.add_argument("--all", action="store_true", help="输出完整日志")
    read.add_argument("--wait", type=float, default=0.0, help="等待新输出秒数")

    for command in (run, write, read):
        command.add_argument("--log", default=DEFAULT_LOG, help=f"输出日志文件（默认 {DEFAULT_LOG}）")
    for command in (run, write):
        command.add_argument("--in-sock", default=DEFAULT_SOCK, help=f"agent 输入 socket（默认 {DEFAULT_SOCK}）")
    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()

    if args.command == "run":
        cmd_run(args)
    elif args.command == "write":
        cmd_write(args)
    elif args.command == "read":
        cmd_read(args)
    return 0


if __name__ == "__main__":
    sys.exit(main())
