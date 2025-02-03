import sys
import pandas as pd
import numpy as np

num_map = {
    "1": "一",
    "2": "二",
    "3": "三",
    "4": "四",
    "5": "五",
}


def number2shuzi(int):
    shuzi = ""
    for map_id in str(int):
        shuzi += num_map[map_id]
    return shuzi


def main():
    if len(sys.argv) < 2:
        print("错误:请提供CSV文件的路径作为命令行参数")
        exit(1)

    csv_path = sys.argv[1]

    # 读取CSV文件(跳过第一行，无表头，保留字符串)
    csv_file = pd.read_csv(
        csv_path,
        sep=",",
        dtype=str,
        header=0,  # 识别表头
    )

    if csv_file.empty:
        print("错误:CSV文件在跳过首行后内容为空")
        exit(1)

    # 转置数据(交换行和列)
    csv_file_transposed = csv_file.transpose()

    # 转换为二维numpy数组(形状为 [原列数, 原行数-1])
    data_2d = csv_file_transposed.to_numpy()

    # !!! 建议在此处插入星期检查代码

    # 构建三维数组 (16, 原列数, 原行数-1)
    three_dim_array = np.repeat(
        data_2d[np.newaxis, :, :],  # 先增加一个维度 (1, 原列数, 原行数-1)
        repeats=16,
        axis=0,
    )

    # 输出验证
    print(f"三维数组形状:(16, 原CSV列数, 原CSV行数-1) = {three_dim_array.shape}")
    print(
        f"示例数据:第一周星期二第一、二节课: {three_dim_array[0, 2, 0]} {three_dim_array[0, 2, 2]}"
    )
    week_count = 2  # 第二周
    for day in range(1, 6):  # 不包括6
        for time_period in range(0, 12):
            print(
                f"星期{number2shuzi(day)}:{three_dim_array[week_count, 0, time_period]}:{three_dim_array[week_count, day, time_period]}"  # 打印具体课程
            )


if __name__ == "__main__":
    main()
