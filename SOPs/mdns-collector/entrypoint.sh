#!/bin/sh
set -eu

# 启动子进程后都放进同一个进程组，tini -g 会把信号广播过来
term_handler() {
  echo "SIGTERM received, shutting down..." >&2
  # 尽量优雅停 avahi/dbus（如果它们还在）
  kill -TERM "${AVAHI_PID:-0}" 2>/dev/null || true
  kill -TERM "${DBUS_PID:-0}" 2>/dev/null || true
  kill -TERM "${PY_PID:-0}" 2>/dev/null || true
}

trap term_handler TERM INT

mkdir -p /run/dbus
dbus-daemon --system --fork --nopidfile
DBUS_PID="$(cat /run/dbus/pid 2>/dev/null || echo "")"

cat > /etc/avahi/avahi-daemon.conf <<'EOF'
[server]
use-ipv4=yes
use-ipv6=no
allow-interfaces=eth0
EOF

avahi-daemon -D --no-drop-root
# Alpine avahi 写 pid 的路径可能不同；我们用 pgrep 兜底
AVAHI_PID="$(pgrep -xo avahi-daemon 2>/dev/null || true)"

# 让 python 作为前台主进程：exec 很关键，但 exec 后 trap 会失效
# 所以我们不用 exec，改为后台跑 python 并 wait 它，这样 trap 能执行清理。
python3 /collector.py &
PY_PID="$!"

wait "$PY_PID"
