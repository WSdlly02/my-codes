FROM python:3.14-alpine

RUN apk add --no-cache \
  avahi avahi-tools dbus \
  && rm -rf /var/lib/apk/lists/*

COPY collector.py /collector.py
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
