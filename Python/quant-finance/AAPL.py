import yfinance as yf
import pandas as pd
import matplotlib.pyplot as plt

# 下载数据
ticker = "AAPL"
df = yf.download(ticker, period="5y", interval="1d")

# 计算短期和长期均线
df["SMA20"] = df["Close"].rolling(window=20).mean()
df["SMA50"] = df["Close"].rolling(window=50).mean()

# 生成交易信号
df["Signal"] = 0
df.loc[df["SMA20"] > df["SMA50"], "Signal"] = 1  # 短期均线在上 → 做多
df.loc[df["SMA20"] < df["SMA50"], "Signal"] = 0  # 短期均线在下 → 空仓

# 计算每日收益
df["Return"] = df["Close"].pct_change()

# 策略收益 = 市场收益 * 持仓信号
df["Strategy_Return"] = df["Return"] * df["Signal"].shift(1)

# 累计资金曲线
df["Cumulative_Market"] = (1 + df["Return"]).cumprod()
df["Cumulative_Strategy"] = (1 + df["Strategy_Return"]).cumprod()

# 可视化
plt.figure(figsize=(12, 6))
plt.plot(df.index, df["Cumulative_Market"], label="Buy & Hold", linewidth=2)
plt.plot(
    df.index, df["Cumulative_Strategy"], label="SMA Crossover Strategy", linewidth=2
)
plt.title(f"{ticker} - SMA20/50 Crossover Backtest")
plt.xlabel("Date")
plt.ylabel("Portfolio Value (Start=1)")
plt.legend()
plt.show()
