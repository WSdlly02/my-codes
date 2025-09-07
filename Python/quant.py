import yfinance as yf
import matplotlib.pyplot as plt

# 1. 下载数据（过去5年）
ticker = "AAPL"
df = yf.download(ticker, period="5y", interval="1d")

# 2. 查看前几行数据
print(df)
print(df.head())

# 3. 画收盘价曲线
plt.figure(figsize=(12, 5))
plt.plot(df.index, df["Close"], label="Close Price")
plt.title(f"{ticker} Closing Price (5y)")
plt.xlabel("Date")
plt.ylabel("Price ($)")
plt.legend()
plt.show()

# 4. 计算每日收益率
df["Return"] = df["Close"].pct_change()

# 5. 计算累计收益曲线（假设初始资金=1）
df["Cumulative"] = (1 + df["Return"]).cumprod()

# 6. 绘制累计收益曲线
plt.figure(figsize=(12, 5))
plt.plot(df.index, df["Cumulative"], label="Cumulative Return")
plt.title(f"{ticker} Cumulative Return (5y)")
plt.xlabel("Date")
plt.ylabel("Portfolio Value (Start=1)")
plt.legend()
plt.show()
