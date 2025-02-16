#!/usr/bin/env bash
while true; do
  for i in {60934..60939}; do
    echo -en "\r$(printf '\\u%x' $i) "
    sleep 0.1
  done
done
