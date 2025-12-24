FROM python:3.14-alpine

RUN apk add --no-cache \
  tini avahi avahi-tools dbus \
  && rm -rf /var/lib/apk/lists/* /var/cache/apk/*

COPY collector.py /collector.py
COPY --chmod=755 entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/sbin/tini","-g","--","/entrypoint.sh"]
