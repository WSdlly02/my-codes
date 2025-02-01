import sys
import pandas as pd
import numpy as np

def main():
    if len(sys.argv) < 2:
        print("错误：请提供CSV文件的路径作为命令行参数。")
        return

    csv_path = sys.argv[1]

    try:
        # 读取CSV文件（跳过第一行，无表头，保留字符串）
        df = pd.read_csv(
            csv_path,
            sep=',',
            dtype=str,
            header=0,       # 识别表头
        )

        if df.empty:
            print("错误：CSV文件在跳过首行后内容为空。")
            return

        # 转置数据（交换行和列）
        df_transposed = df.transpose()

        # 转换为二维numpy数组（形状为 [原列数, 原行数-1]）
        data_2d = df_transposed.to_numpy()

        # 构建三维数组 (16, 原列数, 原行数-1)
        three_dim_array = np.repeat(
            data_2d[np.newaxis, :, :],  # 先增加一个维度 (1, 原列数, 原行数-1)
            repeats=16,
            axis=0
        )

        # 输出验证
        print(f"三维数组形状：(16, 原CSV列数, 原CSV行数-1) = {three_dim_array.shape}")
        print(f"示例数据 [0,0,0]: {three_dim_array[0, 2, 2]} {three_dim_array[0, 2, 0]}")
        # 第一周，星期二，第三个时间段、第一个时间段
        layer_index=2
        target_3d_column_index =2
        for xinqi in range(1,6): 
            for shijianduan in range(0,12):
                print("星期"+str(xinqi)+":"+three_dim_array[layer_index, 0, shijianduan]+":"+three_dim_array[layer_index, xinqi, shijianduan])

    except FileNotFoundError:
        print(f"错误：文件 '{csv_path}' 未找到。")
    except pd.errors.EmptyDataError:
        print("错误：CSV文件内容为空。")
    except Exception as e:
        print(f"运行时发生错误：{e}")

if __name__ == "__main__":
    main()