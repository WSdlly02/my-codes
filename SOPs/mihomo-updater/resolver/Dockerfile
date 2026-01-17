FROM golang:alpine AS builder

RUN apk add --no-cache ca-certificates curl file binutils

ARG TARGETARCH

# Compile the Go binary
WORKDIR /src
COPY go.mod go.sum ./
RUN --mount=type=cache,target=/go/pkg/mod \
  go mod download

COPY main.go ./

ENV CGO_ENABLED=0 \
  GOOS=linux \
  GOARCH=$TARGETARCH \
  GOAMD64=v3 
RUN --mount=type=cache,target=/root/.cache/go-build \
  go build -ldflags="-s -w" -trimpath -o resolver main.go && \
  strip --strip-all resolver && \
  file resolver | grep -q "statically linked" || { echo "Not fully static!"; exit 1; }

# Install yq
RUN curl -fL --retry 3 --retry-delay 1 \
  "https://github.com/mikefarah/yq/releases/latest/download/yq_linux_$TARGETARCH" \
  -o yq && chmod +x yq

FROM scratch

WORKDIR /app

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=builder /src/yq /usr/local/bin/yq
COPY --from=builder /src/resolver .
ENV PATH="/usr/local/bin"

EXPOSE 8088
ENTRYPOINT ["/app/resolver"]