FROM rust:alpine AS builder

WORKDIR /src

COPY Cargo.toml Cargo.lock ./
RUN --mount=type=cache,target=/usr/local/cargo/registry \
  cargo fetch --locked

COPY src src
RUN --mount=type=cache,target=/usr/local/cargo/registry \
  --mount=type=cache,target=/src/target \
  cargo build --locked --release --bin resolver && \
  # Critical! copy the binary to a known location otherwise it will be lost because of the cache mount
  cp /src/target/release/resolver /src/resolver

FROM alpine:latest

RUN apk add --no-cache ca-certificates yq-go

WORKDIR /app

COPY --from=builder /src/resolver /app/resolver

EXPOSE 8088
ENTRYPOINT ["/app/resolver"]
