#!/bin/sh
set -eu

mkdir -p /run/dbus
dbus-daemon --system --fork

# 只监听容器网卡（通常是 eth0）
cat > /etc/avahi/avahi-daemon.conf <<'EOF'
[server]
use-ipv4=yes
use-ipv6=no
allow-interfaces=eth0
EOF

avahi-daemon --daemonize --no-drop-root

exec python3 /collector.py
