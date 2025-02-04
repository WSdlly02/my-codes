import sys
import pandas as pd
import numpy as np

num_map = {
    "1": "一",  # 键必须immutable
    "2": "二",
    "3": "三",
    "4": "四",
    "5": "五",
}


def number2shuzi(day):
    for key in str(day):
        shuzi = num_map[key]
    return shuzi


"""
步骤:
1.获取二维数组中的单个元素,"龙海曌 外国建筑赏析(3:教学3A304);黄杰 思想道德与法治(11-12);董蕾 中国近现代史纲要(9-10)"
2.分隔符";"分割
"""


def split_string_in_column(source):
    content = source.split(";")
    print(content)
    return content


def extract_content_in_parentheses(source):
    start = source.find("(")
    end = source.find(")")

    # 检查括号是否存在
    if start == -1 or end == -1:
        # print(f"错误：字符串 '{source}' 中没有括号或格式不正确。")
        content = source
        return content
    else:
        # 提取括号内的内容
        content = source[start + 1 : end]
        return content


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

    for day in range(1, 6):  # 不包括6,跳过时间列
        for time_period in range(0, 12):  # 13个时间段
            course_detail = extract_content_in_parentheses(
                split_string_in_column(data_2d[day, time_period])
            )
            print(course_detail)
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
                f"第{number2shuzi(week_count)}周星期{number2shuzi(day)}:{three_dim_array[week_count, 0, time_period]}:{three_dim_array[week_count, day, time_period]}"  # 打印具体课程
            )


if __name__ == "__main__":
    main()
