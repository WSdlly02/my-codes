import numpy as np
import matplotlib.pyplot as plt
import matplotlib

matplotlib.rcParams["font.sans-serif"] = ["Sarasa Gothic CL"]
matplotlib.rcParams["axes.unicode_minus"] = False

# 设置参数
C_bike = 1250  # 个人自行车的初始购买费用
C_maintenance = 50  # 个人自行车的年维护费用
C_theft = 12.5  # 每年的被盗风险成本（1% 被盗概率）
price_increase_rate = 0.1  # 共享单车年卡价格上涨率
initial_shared_price = 200  # 共享单车年卡的初始价格


# 计算共享单车的总成本
def shared_cost(n):
    if n == 1:
        return 200
    else:
        return 200 * ((1 + price_increase_rate) ** (n - 1)) + shared_cost(n - 1)


# 计算个人自行车的总成本
def bike_cost(n):
    return C_bike + C_maintenance * n + C_theft * n


# 计算共享单车和个人自行车的总成本，逐年计算
years = np.arange(1, 21)  # 假设使用期为 1 到 20 年
shared_costs = [shared_cost(n) for n in years]
bike_costs = [bike_cost(n) for n in years]

# 绘制图形
plt.figure(figsize=(10, 6))
plt.plot(years, shared_costs, label="共享单车总成本", color="b")
plt.plot(years, bike_costs, label="个人自行车总成本", color="r", linestyle="--")
plt.xlabel("使用年数 (n)")
plt.ylabel("总成本 (元)")
plt.title("共享单车与个人自行车成本对比")
plt.legend()
plt.grid(True)
plt.show()

# 查找两者交点
for n in years:
    print(f"第{n}年: 共享单车{shared_cost(n):.1f}, 个人自行车{bike_cost(n):.1f}")
    if shared_cost(n) >= bike_cost(n):
        break

# 输出交点信息
print(f"共享单车和个人自行车的成本交点在第 {n} 年")
print(
    f"此时共享单车的总成本为 {shared_cost(n):.1f} 元，个人自行车的总成本为 {bike_cost(n):.1f} 元"
)
