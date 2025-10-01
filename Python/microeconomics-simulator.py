import numpy as np
import matplotlib.pyplot as plt
import matplotlib

matplotlib.rcParams["font.sans-serif"] = ["Sarasa Gothic CL"]
matplotlib.rcParams["axes.unicode_minus"] = False

# --- Step 1. 生成买家和卖家 ---
N_buyers = 1000
N_sellers = 1000

# 买家愿付价（reservation value）
buyer_values = np.random.uniform(0, 100, N_buyers)
# 卖家成本（reservation cost）
seller_costs = np.random.uniform(0, 100, N_sellers)

# 排序
buyer_values.sort()
seller_costs.sort()

# --- Step 2. 构建供给/需求函数 ---
price_grid = np.linspace(0, 100, 200)  # 从0到100的价格网格
print(price_grid)
demand = [np.sum(buyer_values >= p) for p in price_grid]
supply = [np.sum(seller_costs <= p) for p in price_grid]

# --- Step 3. 找到均衡点（使 Q_d ≈ Q_s）---
diff = np.abs(np.array(demand) - np.array(supply))
eq_idx = np.argmin(diff)
p_eq = price_grid[eq_idx]
q_eq = (demand[eq_idx] + supply[eq_idx]) / 2

print(f"近似均衡价格: {p_eq:.2f}, 均衡数量: {q_eq}")

# --- Step 4. 画图 ---
plt.figure(figsize=(7, 5))
plt.plot(price_grid, demand, label="需求曲线 (Qd)")
plt.plot(price_grid, supply, label="供给曲线 (Qs)")
plt.scatter(p_eq, q_eq, color="red", zorder=5, label="均衡点")
plt.xlabel("价格")
plt.ylabel("数量")
plt.legend()
plt.title("最小供需均衡模型")
plt.show()
