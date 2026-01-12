FROM golang:alpine AS builder

WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY *.go ./
RUN GOAMD64=v3 CGO_ENABLED=0 go build -ldflags="-s -w" -trimpath -o resolver .

FROM alpine:latest

WORKDIR /app

# Install yq
RUN apk add --no-cache curl ca-certificates && \
  curl -L https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -o /usr/bin/yq && \
  chmod +x /usr/bin/yq

COPY --from=builder /app/resolver .

# .env file should be exposed when running

EXPOSE 8088

# Use shell form or exec form. Exec form is better for signals.
ENTRYPOINT ["./resolver"]