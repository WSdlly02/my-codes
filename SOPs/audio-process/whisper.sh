#!/usr/bin/env bash

# 镜像名称
IMAGE_NAME="openai-whisper:rocm"
# 获取 Dockerfile 的绝对路径
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKERFILE="$SCRIPT_DIR/Dockerfile.openai-whsiper:rocm"

# 如果镜像不存在，则自动构建
if ! docker image inspect "$IMAGE_NAME" >/dev/null 2>&1; then
    echo "正在构建镜像 $IMAGE_NAME..."
    docker pull ghcr.io/wsdlly02/my-codes/openai-whisper:rocm || docker build -t "$IMAGE_NAME" -f "$DOCKERFILE" "$SCRIPT_DIR"
fi

# 持久化模型缓存路径
CACHE_DIR="$HOME/.cache/whisper"
mkdir -p "$CACHE_DIR"

docker run --rm \
    --network=host \
    --device=/dev/kfd \
    --device=/dev/dri \
    --group-add=video \
    --ipc=host \
    --cap-add=SYS_PTRACE \
    --security-opt seccomp=unconfined \
    -e HSA_OVERRIDE_GFX_VERSION=10.3.0 \
    -v "$CACHE_DIR":/tmp/.cache/whisper \
    -e XDG_CACHE_HOME=/tmp/.cache \
    -e HOME=/tmp \
    -v "$PWD":"$PWD" \
    -w "$PWD" \
    "$IMAGE_NAME" "$@"
