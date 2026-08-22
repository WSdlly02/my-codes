#!/usr/bin/env bash
# 优雅停止 REPL：先给 REPL 发 quit 让其自行退出，兜底再杀 driver 进程。
set -euo pipefail
DRIVER_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOCK="${COURSE_REPL_SOCK:-/tmp/course-repl.sock}"

if [ -S "$SOCK" ]; then
    printf 'quit\n' | python3 "$DRIVER_DIR/pty_client.py" "$SOCK" --idle 1 --max 10 || true
    sleep 0.5
fi
# 兜底：杀掉对应的 pty_driver。[.] 防止 pkill 匹配到本脚本自身的命令行。
pkill -f "pty_driver[.]py --socket ${SOCK}" 2>/dev/null || true
rm -f "$SOCK"
echo "REPL 已停止"
