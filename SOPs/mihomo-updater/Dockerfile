FROM rust:alpine AS builder

RUN apk add --no-cache build-base musl-dev ca-certificates file binutils

WORKDIR /src

COPY Cargo.toml Cargo.lock ./
RUN --mount=type=cache,target=/usr/local/cargo/registry \
  cargo fetch --locked

COPY src src
RUN --mount=type=cache,target=/usr/local/cargo/registry \
  --mount=type=cache,target=/src/target \
  cargo build --locked --release --bin resolver && \
  strip --strip-all target/release/resolver && \
  file target/release/resolver | grep -q "statically linked" || { echo "Not fully static!"; exit 1; }

FROM alpine:latest

RUN apk add --no-cache ca-certificates yq-go

WORKDIR /app

COPY --from=builder /src/target/release/resolver /app/resolver

EXPOSE 8088
ENTRYPOINT ["/app/resolver"]
