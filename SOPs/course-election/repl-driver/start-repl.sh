#!/usr/bin/env bash
# 启动 course-election REPL：后台常驻，通过 pty_driver 暴露 Unix socket。
set -euo pipefail
DRIVER_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$DRIVER_DIR")"
SOCK="${COURSE_REPL_SOCK:-/tmp/course-repl.sock}"

if [ -S "$SOCK" ]; then
    echo "已有会话在运行（$SOCK）。如需重启：先执行 stop-repl.sh" >&2
    exit 1
fi

nohup python3 "$DRIVER_DIR/pty_driver.py" \
    --socket "$SOCK" \
    --cwd "$PROJECT_DIR" \
    --cmd "$PROJECT_DIR/course-election" \
    > /tmp/course_repl_driver.log 2>&1 &

echo "driver pid=$!"
sleep 1
if [ -S "$SOCK" ]; then
    echo "REPL 已启动: $SOCK"
    echo "测试: printf 'status\\n' | python3 $DRIVER_DIR/pty_client.py $SOCK --wait 'course-election> '"
else
    echo "启动失败，查看 /tmp/course_repl_driver.log" >&2
    exit 1
fi
