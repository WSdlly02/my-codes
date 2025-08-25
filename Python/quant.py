import pandas as pd
import matplotlib.pyplot as plt
import yfiance as yf

# 读取股票数据（假设有一列 Date 和 Close）
# 你可以去 Yahoo Finance 下载 CSV，例如 AAPL.csv
df = pd.read_csv("AAPL.csv", parse_dates=["Date"], index_col="Date")

# 画出收盘价
plt.figure(figsize=(10, 5))
df["Close"].plot()
plt.title("Apple Stock Price")
plt.ylabel("Price ($)")
plt.xlabel("Date")
plt.show()
