-- ThermalProcessorBinary.hs
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever, replicateM) -- 添加此导入
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO (BufferMode (..), hSetBuffering, stdin)

-- 数据解析函数保持不变
parseThermalData :: Get (Double, [Float])
parseThermalData = do
  timestamp <- getDoublele
  temperatures <- replicateM 768 getFloatle -- 现在 replicateM 已可用
  return (timestamp, temperatures)

jetColor :: Float -> (Int, Int, Int)
jetColor x = (round (clamp r * 255), round (clamp g * 255), round (clamp b * 255))
  where
    -- 分段计算RGB分量
    (r, g, b)
      | x < 0.125 = (0, 0, 0.5 + 4 * x) -- 深蓝到蓝
      | x < 0.375 = (0, 4 * (x - 0.125), 1.0) -- 蓝到青
      | x < 0.625 = (4 * (x - 0.375), 1.0, 1.0 - 4 * (x - 0.375)) -- 青到黄
      | x < 0.875 = (1.0, 1.0 - 4 * (x - 0.625), 0) -- 黄到红
      | otherwise = (max (1.0 - 4 * (x - 0.875)) 0.5, 0, 0) -- 红到暗红
      -- 确保颜色值在0-1范围内
    clamp = max 0 . min 1

-- 主程序
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  let chunkSize = 8 + 4 * 768

  forever $ do
    bytes <- B.hGet stdin chunkSize
    case runGetOrFail parseThermalData (BL.fromStrict bytes) of
      Left (_, _, err) ->
        putStrLn $ "解析错误: " ++ err
      Right (_, _, (ts, temps)) -> do
        let maxTemp = maximum temps -- 求最大温度
        let tempNormalization = normalize temps where
            normalize [] = []
            normalize (x : xs)
              | x < 10 = 0 : normalize xs
              | x < 40 = ((x - 10) / 30) : normalize xs
              | otherwise = 1 : normalize xs
        let tempColorMap = map jetColor tempNormalization
        -- let tempArray = array2dim temps where -- 将768个温度数据存进32*24的二维矩阵
        --     array2dim [] = []
        --     array2dim (xs) = take 32 (xs) : array2dim (drop 32 xs)
        -- let warnTempArray = refill2DimArrayWithTemp tempArray where -- 过滤矩阵，记录危险温度
        --     refill2DimArrayWithTemp [] = []
        --     refill2DimArrayWithTemp (x:xs) = refillArrayWithTemp x : refill2DimArrayWithTemp xs where
        --         refillArrayWithTemp [] = []
        --         refillArrayWithTemp (y:ys)
        --             | y<30 = 0 : refillArrayWithTemp ys
        --             | y<40 = 1 : refillArrayWithTemp ys
        --             | otherwise = 2 : refillArrayWithTemp ys
        putStrLn $ "时间戳: " ++ show ts
        putStrLn $ "最高温度: " ++ show maxTemp ++ "°C"
        putStrLn $ "所有温度热力图: " ++ show tempColorMap
        -- putStrLn $ "警告温度: " ++ show warnTempArray
        putStrLn "--------------------------"
