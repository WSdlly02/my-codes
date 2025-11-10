"""
一个Python脚本，用于递归查找文件夹中的重复文件，
并仅保留每个重复组中修改时间最早的一个。
"""

import sys
import os
import hashlib
import collections
from datetime import datetime


def hash_file(filepath):
    """
    计算文件的SHA-256哈希值。
    为了处理大文件，它会分块读取。
    如果文件无法读取，返回None。
    """
    sha256 = hashlib.sha256()
    try:
        with open(filepath, "rb") as f:
            # 以64k的块读取文件，防止内存溢出
            while chunk := f.read(65536):
                sha256.update(chunk)
            return sha256.hexdigest()
    except IOError as e:
        # 处理读取错误（例如，权限问题）
        print(f"警告: 无法读取文件 {filepath} ({e})", file=sys.stderr)
        return None


def find_files(directory):
    """
    递归地遍历目录并生成所有文件的完整路径。
    """
    for root, _, filenames in os.walk(directory):
        for filename in filenames:
            filepath = os.path.join(root, filename)
            # 确保它是一个文件，而不是一个（损坏的）符号链接
            if os.path.isfile(filepath):
                yield filepath


def group_files_by_hash(filepaths):
    """
    接收文件路径列表，按哈希值对它们进行分组。
    值中存储 (filepath, modification_time) 元组。
    """
    hashes = collections.defaultdict(list)
    total_files = len(filepaths)

    for i, filepath in enumerate(filepaths):
        # 打印进度
        basename = os.path.basename(filepath)
        # 截断文件名以确保显示不会过长（37 + ... = 40）
        truncated_name = (basename[:37] + "...") if len(basename) > 40 else basename

        # 格式化进度字符串
        progress_text = f"\r正在处理文件: {i+1}/{total_files} ({truncated_name:<40})..."

        # 打印进度文本，并用空格填充到固定宽度（例如100个字符）
        # 这可以确保清除上一行可能留下的任何残留字符。
        print(f"{progress_text:<100}", end="")

        file_hash = hash_file(filepath)
        if file_hash:
            try:
                mod_time = os.path.getmtime(filepath)
                hashes[file_hash].append((filepath, mod_time))
            except OSError as e:
                print(f"\n警告: 无法获取文件时间 {filepath} ({e})", file=sys.stderr)

    print("\n文件哈希计算完成。")
    return hashes


def process_duplicates(hash_groups):
    """
    处理按哈希分组的文件。
    找到最老的文件并删除其余的文件。
    """
    found_duplicates = False

    for files_list in hash_groups.values():
        if len(files_list) > 1:
            found_duplicates = True

            # 按修改时间 (item[1]) 排序，找到最老的文件
            oldest_file = min(files_list, key=lambda item: item[1])
            oldest_path, oldest_time_stamp = oldest_file

            # 将时间戳转换为可读格式
            oldest_time = datetime.fromtimestamp(oldest_time_stamp).strftime(
                "%Y-%m-%d %H:%M:%S"
            )

            print(f"\n发现一组重复文件 ({os.path.basename(oldest_path)}):")
            print(f"  - [保留] {oldest_path} (最早修改时间: {oldest_time})")

            # 遍历并删除除最老文件之外的所有文件
            for filepath, _ in files_list:
                if filepath != oldest_path:
                    try:
                        os.remove(filepath)
                        print(f"  - [删除] {filepath}")
                    except OSError as e:
                        print(f"  - [错误] 无法删除 {filepath} ({e})", file=sys.stderr)

    return found_duplicates


def main():
    """
    程序主入口。
    """
    if len(sys.argv) != 2:
        print(f"用法: python3 {sys.argv[0]} <文件夹路径>", file=sys.stderr)
        sys.exit(1)

    directory = sys.argv[1]

    if not os.path.isdir(directory):
        print(f"错误: 路径 '{directory}' 不是一个有效的文件夹。", file=sys.stderr)
        sys.exit(1)

    print(f"正在扫描文件夹: {directory}")

    # 获取所有文件路径
    all_files = list(find_files(directory))
    print(f"找到 {len(all_files)} 个文件。")

    if not all_files:
        print("文件夹为空，无需操作。")
        return

    # 按哈希分组
    hash_groups = group_files_by_hash(all_files)

    # 处理重复文件
    found_duplicates = process_duplicates(hash_groups)

    if not found_duplicates:
        print("未找到重复文件。")

    print("清理完成。")


if __name__ == "__main__":
    main()
