import yfinance as yf
import pandas as pd
import matplotlib.pyplot as plt

# 1. 获取数据
ticker_symbol = "AAPL"
start_date = "2020-01-01"
end_date = "2024-12-31"
aapl_data = yf.download(ticker_symbol, start=start_date, end=end_date)

# 2. 计算指标 (Vectorized)
aapl_data["sma_20"] = aapl_data["Close"].rolling(window=20).mean()
aapl_data["sma_50"] = aapl_data["Close"].rolling(window=50).mean()

# 3. 生成状态和信号 (Vectorized)
# 当 sma_20 > sma_50 时为 1, 否则为 0
aapl_data["position"] = (aapl_data["sma_20"] > aapl_data["sma_50"]).astype(int)
# 1 表示金叉买入, -1 表示死叉卖出
aapl_data["signal"] = aapl_data["position"].diff()

# 4. 可视化
plt.figure(figsize=(14, 7))

# 价格与均线图
plt.subplot(2, 1, 1)  # 创建一个2行1列的图表，当前是第1个
plt.plot(aapl_data.index, aapl_data["Close"], label="Close Price", alpha=0.7)
plt.plot(aapl_data.index, aapl_data["sma_20"], label="SMA 20", linestyle="--")
plt.plot(aapl_data.index, aapl_data["sma_50"], label="SMA 50", linestyle="--")
# 标记交易信号点
plt.plot(
    aapl_data[aapl_data["signal"] == 1].index,
    aapl_data["sma_20"][aapl_data["signal"] == 1],
    "^",
    markersize=10,
    color="g",
    label="Buy Signal",
)
plt.plot(
    aapl_data[aapl_data["signal"] == -1].index,
    aapl_data["sma_20"][aapl_data["signal"] == -1],
    "v",
    markersize=10,
    color="r",
    label="Sell Signal",
)
plt.title(f"{ticker_symbol} Moving Average Crossover Strategy")
plt.ylabel("Price ($)")
plt.legend()

# 信号图
plt.subplot(2, 1, 2)  # 当前是第2个图
plt.plot(aapl_data.index, aapl_data["signal"], marker="o", linestyle="")
plt.title("Trading Signals")
plt.ylabel("Signal (1=Buy, -1=Sell)")
plt.xlabel("Date")

plt.tight_layout()  # 调整子图布局
plt.show()

# 打印有信号的交易日
print("Trading signals:")
print(aapl_data[aapl_data["signal"] != 0]["signal"])
