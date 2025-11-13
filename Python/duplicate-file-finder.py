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

    优化：使用1MB块大小，减少系统调用次数。
    """
    sha256 = hashlib.sha256()
    try:
        with open(filepath, "rb", buffering=1048576) as f:  # 1MB缓冲区
            # 以1MB的块读取文件，减少系统调用
            while chunk := f.read(1048576):
                sha256.update(chunk)
            return sha256.hexdigest()
    except IOError:
        # 静默处理读取错误以提高性能
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


def get_file_size(filepath):
    """
    获取文件大小（字节数）。
    如果失败返回None。
    """
    try:
        return os.path.getsize(filepath)
    except OSError:
        return None


def group_files_by_size(filepaths):
    """
    ★★★ 关键优化：先按文件大小分组 ★★★
    只有大小相同的文件才可能是重复的。
    这可以大幅减少需要计算哈希的文件数量。
    """
    size_groups = collections.defaultdict(list)

    for filepath in filepaths:
        size = get_file_size(filepath)
        if size is not None:
            size_groups[size].append(filepath)

    # 只返回有多个文件的大小组（可能的重复）
    potential_duplicates = []
    for files_list in size_groups.values():
        if len(files_list) > 1:
            potential_duplicates.extend(files_list)

    return potential_duplicates


def group_files_by_hash(filepaths):
    """
    接收文件路径列表，按哈希值对它们进行分组。
    值中存储 (filepath, modification_time) 元组。

    优化：移除进度输出以提高性能。
    """
    hashes = collections.defaultdict(list)

    for filepath in filepaths:
        file_hash = hash_file(filepath)
        if file_hash:
            try:
                mod_time = os.path.getmtime(filepath)
                hashes[file_hash].append((filepath, mod_time))
            except OSError:
                pass  # 静默忽略以提高性能

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

    # 获取所有文件路径
    all_files = list(find_files(directory))

    if not all_files:
        print("文件夹为空，无需操作。")
        return

    # ★★★ 关键优化：先按文件大小过滤 ★★★
    # 只对大小相同的文件计算哈希
    potential_duplicates = group_files_by_size(all_files)

    # 如果没有大小相同的文件，直接结束
    if not potential_duplicates:
        print("未找到重复文件。")
        return

    # 按哈希分组（仅对可能重复的文件）
    hash_groups = group_files_by_hash(potential_duplicates)

    # 处理重复文件
    found_duplicates = process_duplicates(hash_groups)

    if not found_duplicates:
        print("未找到重复文件。")
    else:
        print("清理完成。")


if __name__ == "__main__":
    main()
