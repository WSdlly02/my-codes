FROM golang:alpine AS builder

ARG TARGETARCH

# Compile the Go binary
WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY *.go ./
RUN CGO_ENABLED=0 GOOS=linux GOARCH=$TARGETARCH GOAMD64=v3 \
  go build -ldflags="-s -w" -trimpath -o resolver .

# Install yq
RUN apk add --no-cache curl ca-certificates && \
  curl -L "https://github.com/mikefarah/yq/releases/latest/download/yq_linux_$TARGETARCH" -o yq && \
  chmod +x yq

FROM alpine:latest

WORKDIR /app
COPY --from=builder /app/yq /usr/local/bin/yq
COPY --from=builder /app/resolver .

# .env file should be exposed when running
EXPOSE 8088
ENTRYPOINT ["./resolver"]