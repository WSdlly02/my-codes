#!/bin/sh
set -eu

mkdir -p /run/dbus

# system bus：不要 --fork
dbus-daemon --system --nopidfile --nofork &
DBUS_PID=$!

# avahi 只监听 eth0/IPv4
cat > /etc/avahi/avahi-daemon.conf <<'EOF'
[server]
use-ipv4=yes
use-ipv6=no
allow-interfaces=eth0
EOF

# avahi：不要 --daemonize
avahi-daemon --no-drop-root &
AVAHI_PID=$!

# python 在前台（tini 会把 SIGTERM 发给它；它退出容器就退出）
exec python3 /collector.py
