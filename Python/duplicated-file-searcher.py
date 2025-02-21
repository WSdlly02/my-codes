import os
import sys

if len(sys.argv) < 2:
    print("错误:请提供文件夹路径作为命令行参数")
    exit(1)

给定文件夹 = sys.argv[1]
文件绝对路径数组 = []


def 遍历文件(给定文件夹):
    # 首先遍历当前目录所有文件及文件夹
    文件相对路径数组 = os.listdir(给定文件夹)
    # 循环判断每个元素是否是文件夹还是文件
    for file in 文件相对路径数组:
        # 利用os.path.join()方法取得路径全名，并存入"文件绝对路径"变量，否则每次只能遍历一层目录
        文件绝对路径 = os.path.join(给定文件夹, file)
        # 判断是否是文件夹
        if os.path.isdir(文件绝对路径) == False:
            文件绝对路径数组.append(文件绝对路径)


def 寻找冗余(文件大小数组):
    冗余文件数组 = []
    for size in 文件大小数组:
        if 文件大小数组.count(size) > 1 and size not in 冗余文件数组:
            冗余文件数组.append(size)
    return 冗余文件数组


def main():
    遍历文件(给定文件夹)
    count = 0
    文件大小数组 = []
    for file_name in 文件绝对路径数组:
        # 获取每个文件大小
        文件大小数组.append(os.path.getsize(文件绝对路径数组[count]))
        # print(f"文件:{file_name}:{file_size[count]}")
        count += 1
    # print(f"冗余文件数组:{find_duplicates(file_size)}")
    for i in range(len(寻找冗余(文件大小数组))):
        冗余文件索引 = 文件大小数组.index(寻找冗余(文件大小数组)[i])
        print(f"冗余文件:{文件绝对路径数组[冗余文件索引]}")
        os.remove(文件绝对路径数组[冗余文件索引])


if __name__ == "__main__":
    main()
