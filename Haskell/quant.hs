-- file: Main.hs
module Main where

import Data.List (tails)
import System.Random (mkStdGen, randomRs)
import Text.Printf (printf)

-- 生成几何随机游走价格（合成数据，便于无依赖演示）
-- 参数：seed, nDays, startPrice, 年化收益mu, 年化波动vol
generatePrices :: Int -> Int -> Double -> Double -> Double -> [Double]
generatePrices seed nDays start mu vol =
  let g = mkStdGen seed
      u = take nDays (randomRs (-0.5, 0.5) g :: [Double]) -- 近似噪声
      muD = mu / 252.0
      volD = vol / sqrt 252.0
      rets = map (\x -> muD + volD * x) u
   in scanl (\p r -> p * exp r) start rets

-- 简单移动平均（返回与价格等长，前(k-1)个为 Nothing）
sma :: Int -> [Double] -> [Maybe Double]
sma k xs
  | k <= 0 = replicate (length xs) Nothing
  | otherwise =
      let windows = [take k t | t <- tails xs, length t >= k]
          avgs = map (\w -> sum w / fromIntegral k) windows
       in replicate (k - 1) Nothing ++ map Just avgs

-- 由短长均线生成信号：短>长 -> 1（做多），否则 0（空仓）
signals :: [Maybe Double] -> [Maybe Double] -> [Int]
signals s l =
  let step (Just a, Just b) = if a > b then 1 else 0
      step _ = 0
   in zipWith (curry step) s l

-- 日对数收益
logReturns :: [Double] -> [Double]
logReturns ps = zipWith (\p0 p1 -> log (p1 / p0)) ps (drop 1 ps)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

stdev :: [Double] -> Double
stdev xs
  | n <= 1 = 0
  | otherwise =
      let m = mean xs
       in sqrt (sum (map (\x -> let d = x - m in d * d) xs) / fromIntegral (n - 1))
 where
  n = length xs

maxDrawdown :: [Double] -> Double
maxDrawdown equity =
  let runMax = scanl1 max equity
      dds = zipWith (\e m -> e / m - 1.0) equity runMax
   in minimum dds

-- 回测核心：信号滞后一日，换仓日扣成本
-- cost 为每次换仓的“一边”成本（10 bps = 0.001）
backtest ::
  [Double] ->
  Int ->
  Int ->
  Double ->
  (Double, Double, Double, Double, Double, [Int])
backtest prices shortW longW cost =
  let s = sma shortW prices
      l = sma longW prices
      sig = signals s l -- 长度 n
      rets = logReturns prices -- 长度 n-1
      pos = init sig -- 区间[t->t+1]使用 t 日信号（避免前视偏差）
      chg = zipWith (\a b -> if a == b then 0 else 1) (init sig) (drop 1 sig) -- 长度 n-1
      strat =
        zipWith
          (-)
          (zipWith (\p r -> fromIntegral p * r) pos rets)
          (map (\c -> fromIntegral c * cost) chg)
      cum = scanl (+) 0 strat
      equity = map exp cum
      n = length rets
      yrs = fromIntegral n / 252.0
      total = exp (sum strat) - 1.0
      cagr = exp (sum strat / max yrs 1e-9) - 1.0
      volA = stdev strat * sqrt 252.0
      muA = mean strat * 252.0
      sharpe = if volA == 0 then 0 else muA / volA
      mdd = maxDrawdown equity
   in (total, cagr, volA, sharpe, mdd, sig)

percent :: Double -> String
percent x = printf "%.2f%%" (x * 100)

main :: IO ()
main = do
  let seed = 42
      nDays = 1000
      start = 100.0
      muAnn = 0.08
      volAnn = 0.20
      shortW = 20
      longW = 100
      costBps = 10.0 -- 单边 10 bps
      cost = costBps / 10000.0 -- 在对数收益空间近似扣减
      prices = generatePrices seed nDays start muAnn volAnn
      (tot, cagr, vola, sharpe, mdd, sig) = backtest prices shortW longW cost

  putStrLn "=== Moving-Average Crossover Backtest (synthetic data) ==="
  printf "Days: %d  Start: %.2f  mu: %.2f%%  vol: %.2f%%\n" nDays start (muAnn * 100) (volAnn * 100)
  printf "Short W: %d  Long W: %d  Cost: %.1f bps per change\n" shortW longW costBps
  putStrLn "----------------------------------------------------------"
  putStrLn $ "Total Return: " ++ percent tot
  putStrLn $ "CAGR:         " ++ percent cagr
  putStrLn $ "Volatility:   " ++ percent vola
  putStrLn $ "Sharpe:       " ++ printf "%.2f" sharpe
  putStrLn $ "Max Drawdown: " ++ percent mdd
  putStrLn "Last 10 signals (0=flat,1=long):"
  print (take 10 (reverse sig))
