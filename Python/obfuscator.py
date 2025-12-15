#!/usr/bin/env python3

"""
一个用于混淆输入以保护隐私和安全的命令行工具。
此脚本是 Haskell 版本的 Python 实现。
"""

import argparse
import hmac
import hashlib
import os
import sys
from pathlib import Path

# --- 字符集定义 ---
LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHANUM = LETTERS + "0123456789"
SPECIAL_CHARS = "+-/@#%!"


def get_key(key_path: Path) -> bytes:
    """
    获取密钥。如果文件不存在，则生成一个新密钥。
    """
    try:
        # 确保父目录存在
        key_path.parent.mkdir(parents=True, exist_ok=True)

        if key_path.exists():
            if key_path.is_dir():
                print(f"错误: 密钥路径 {key_path} 是一个目录。", file=sys.stderr)
                sys.exit(1)

            content = key_path.read_bytes()

            if not content:
                print(f"错误: 密钥文件为空: {key_path}", file=sys.stderr)
                sys.exit(1)

            if len(content) > 4096:
                print(f"警告: 密钥文件 {key_path} 超过 4096 字节。", file=sys.stderr)

            return content
        else:
            # 文件不存在，生成新密钥
            print(f"未找到密钥文件。正在生成新的 4096 字节密钥于: {key_path}")
            new_key = os.urandom(4096)
            key_path.write_bytes(new_key)
            return new_key

    except IOError as e:
        print(f"处理密钥文件时出错: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"发生意外错误: {e}", file=sys.stderr)
        sys.exit(1)


def write_output(mode: str, output_path: Path, input_str: str, result: str) -> None:
    """
    将结果追加到输出文件。
    """
    try:
        # 确保父目录存在
        output_path.parent.mkdir(parents=True, exist_ok=True)

        final_path = output_path
        if output_path.is_dir():
            print(
                f"警告: 输出路径 {output_path} 是一个目录，将写入默认文件。",
                file=sys.stderr,
            )
            final_path = output_path / "obfuscator-list.txt"

        content = f"Mode: {mode}, Input: {input_str}, Output: {result}\n"

        with open(final_path, "a", encoding="utf-8") as f:
            f.write(content)

        print(f"结果已追加到: {final_path}")

    except IOError as e:
        print(f"写入输出文件时出错: {e}", file=sys.stderr)
        sys.exit(1)


# ==================================================================
# 核心生成函数
# ==================================================================


def generate_port(input_str: str, raw_hash: bytes) -> str:
    """
    模式实现：生成安全端口号 (1024-65535)
    """
    try:
        port_num = int(input_str)
        if not (0 <= port_num <= 65535):
            raise ValueError()
    except ValueError:
        print(f"错误: 端口输入必须是 0-65535 之间的有效数字。", file=sys.stderr)
        sys.exit(1)

    # 从哈希中提取一个大整数 (使用前8个字节)
    large_integer = int.from_bytes(raw_hash[:8], "big")

    # 映射到目标范围
    port_range = 65535 - 1024 + 1
    new_port = 1024 + (large_integer % port_range)
    return str(new_port)


def generate_alpha_num_string(length: int, raw_hash: bytes) -> str:
    """
    为 Other 模式生成纯字母数字字符串
    """
    hash_len = len(raw_hash)
    result = []
    for i in range(length):
        # 安全地从字节生成字符
        byte_val = raw_hash[i % hash_len]
        result.append(ALPHANUM[byte_val % len(ALPHANUM)])
    return "".join(result)


def generate_password_string(length: int, raw_hash: bytes) -> str:
    """
    为 Password 模式生成包含2个特殊字符的密码
    """
    hash_len = len(raw_hash)

    def get_byte(idx: int) -> int:
        """辅助函数，用于安全地从哈希字节中获取一个值"""
        return raw_hash[idx % hash_len]

    # 1. 确定两个不重复的、用于插入特殊字符的位置
    pos1 = get_byte(0) % length
    pos2_prime = get_byte(1) % (length - 1)
    pos2 = pos2_prime + 1 if pos2_prime >= pos1 else pos2_prime

    # 2. 确定要使用的两个特殊字符
    special_char1 = SPECIAL_CHARS[get_byte(2) % len(SPECIAL_CHARS)]
    special_char2 = SPECIAL_CHARS[get_byte(3) % len(SPECIAL_CHARS)]

    # 3. 将特殊字符和它们的位置放入一个字典中，便于查找
    special_map = {pos1: special_char1, pos2: special_char2}

    # 4. 生成最终的密码字符串
    result = []
    for i in range(length):
        if i in special_map:
            result.append(special_map[i])
        else:
            # 从哈希字节索引4开始为字母数字字符获取随机性
            byte_val = get_byte(i + 4)
            result.append(ALPHANUM[byte_val % len(ALPHANUM)])

    return "".join(result)


def generate_id_string(length: int, raw_hash: bytes) -> str:
    """
    为 Id 模式生成专用字符串，确保前两位是字母
    """
    hash_len = len(raw_hash)

    def byte_to_char(char_set: str, idx: int) -> str:
        """安全地从字节生成字符"""
        byte_val = raw_hash[idx % hash_len]
        return char_set[byte_val % len(char_set)]

    # 生成ID的各个部分
    char1 = byte_to_char(LETTERS, 0)
    char2 = byte_to_char(LETTERS, 1)

    rest = [byte_to_char(ALPHANUM, i) for i in range(2, length)]

    return char1 + char2 + "".join(rest)


def main_generator(mode: str, length: int, input_str: str, raw_hash: bytes) -> str:
    """
    根据模式调用相应的生成函数
    """
    if mode == "port":
        return generate_port(input_str, raw_hash)
    elif mode == "password":
        return generate_password_string(length, raw_hash)
    elif mode == "id":
        return generate_id_string(length, raw_hash)
    elif mode == "other":
        return generate_alpha_num_string(length, raw_hash)
    else:
        # 这不应该发生，因为 argparse 已经做了验证
        raise ValueError(f"未知的模式: {mode}")


def main():
    """
    主函数：解析参数并执行主逻辑
    """
    home_dir = Path.home()
    default_key_path = home_dir / ".ssh" / "obfuscator-key"
    default_output_path = home_dir / "Documents" / "obfuscator-list.txt"

    parser = argparse.ArgumentParser(
        description="一个用于混淆输入以保护隐私和安全的命令行工具。",
        formatter_class=argparse.RawTextHelpFormatter,
    )

    parser.add_argument(
        "-m",
        "--mode",
        choices=["port", "password", "id", "other"],
        default="id",
        help="处理模式: port, password, id, other (默认: id)",
    )
    parser.add_argument("-l", "--length", type=int, help="输出长度 (4-128)。")
    parser.add_argument(
        "-k",
        "--key",
        type=Path,
        default=default_key_path,
        help=f"哈希密钥文件的路径。\n(默认: {default_key_path})",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=default_output_path,
        help=f"输出文件的路径。\n(默认: {default_output_path})",
    )
    parser.add_argument("input", help="要处理的输入字符串 (例如，端口号、域名等)。")

    args = parser.parse_args()

    # --- 验证和设置最终参数 ---
    final_mode = args.mode

    # 根据模式处理默认长度
    if args.length is not None:
        if final_mode == "port":
            print("错误: 'port' 模式下不能指定长度。", file=sys.stderr)
            sys.exit(1)
        if not (4 <= args.length <= 128):
            print("错误: 长度必须在 4 和 128 之间。", file=sys.stderr)
            sys.exit(1)
        final_length = args.length
    else:
        if final_mode == "port":
            final_length = 4  # 占位，不使用
        elif final_mode == "password":
            final_length = 16
        elif final_mode == "id":
            final_length = 8
        else:  # other
            final_length = 16

    final_key_path = args.key
    final_output_path = args.output

    # 验证和处理输入
    if not args.input.isascii():
        print("错误: 输入字符串必须只包含 ASCII 字符。", file=sys.stderr)
        sys.exit(1)

    final_input = args.input
    if final_mode == "password":
        final_input = f"{args.input}-password"

    # --- 执行核心逻辑 ---
    try:
        key = get_key(final_key_path)

        # 计算 HMAC-SHA512
        input_bytes = final_input.encode("utf-8")
        hashed_input = hmac.new(key, input_bytes, hashlib.sha512).digest()

        # 生成结果
        result = main_generator(final_mode, final_length, args.input, hashed_input)

        # 打印到控制台
        print(f"Mode: {final_mode}\nInput: {args.input}\nOutput: {result}\n")

        # 写入文件
        write_output(final_mode, final_output_path, args.input, result)

    except Exception as e:
        print(f"程序执行出错: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
