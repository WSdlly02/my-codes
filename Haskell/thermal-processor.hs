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

normalize :: [Float] -> [Float] -- 温度数据归一函数
normalize [] = []
normalize (x : xs)
    | x <= 10 = 0 : normalize xs
    | x <= 40 = ((x - 10) / 30) : normalize xs -- 主要监控10-40度
    | otherwise = 1 : normalize xs

jetColor :: Float -> (Int, Int, Int) -- 温度数据映射至RGB
jetColor x = (round (clamp r * 255), round (clamp g * 255), round (clamp b * 255))
  where
    -- 分段计算RGB分量
    (r, g, b)
        | x < 0.125 = (0, 0, 0.5 + 4 * x) -- 深蓝到蓝
        | x < 0.375 = (0, 4 * (x - 0.125), 1.0) -- 蓝到青
        | x < 0.625 = (4 * (x - 0.375), 1.0, 1.0 - 4 * (x - 0.375)) -- 青到黄
        | x < 0.875 = (1.0, 1.0 - 4 * (x - 0.625), 0) -- 黄到红
        | x <= 1 = (max (1.0 - 4 * (x - 0.875)) 0.5, 0, 0) -- 红到暗红
        | otherwise = (1, 1, 1) -- 将特殊值映射成白色
    clamp = max 0 . min 1

tempRecogAlgo :: [[Int]] -> [[Int]] -- 危险温度矩阵转换图像识别
tempRecogAlgo [] = []
tempRecogAlgo (x : xs) = tempRecogAlgo_step2 x : tempRecogAlgo xs -- 脱一层外壳
  where
    tempRecogAlgo_step2 ys = map (\n -> encircleAlgo ys n) [1 .. 30]
      where
        encircleAlgo ys n
            | n - 1 == 0 && ys !! (n - 1) /= 0 = 3 -- 边界包围
            | n + 1 == 31 && ys !! (n + 1) /= 0 = 3 -- 边界包围
            | ys !! n == ys !! (n - 1) && ys !! n < ys !! (n + 1) = 3 -- 002
            | ys !! n == ys !! (n - 1) && ys !! n == ys !! (n + 1) = ys !! n -- 222/000
            | ys !! n < ys !! (n - 1) && ys !! n == ys !! (n + 1) = 3 -- 200
            | ys !! n > ys !! (n - 1) && ys !! n == ys !! (n + 1) = ys !! n -- 022
            | ys !! n == ys !! (n - 1) && ys !! n > ys !! (n + 1) = ys !! n -- 220
            | otherwise = ys !! n

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
                let tempNormalization = normalize temps -- 将温度数据归一至0-1
                let tempColorMap = map jetColor tempNormalization -- 将归一化的数据转为RGB
                let tempArray = arrayaTo2Dim temps where -- 将768个温度数据存进32*24的二维矩阵
                    arrayaTo2Dim [] = []
                    arrayaTo2Dim (xs) = take 32 (xs) : arrayaTo2Dim (drop 32 xs)
                let warnTempArray = refill2DimArrayWithTemp tempArray where -- 过滤矩阵，记录危险温度
                    refill2DimArrayWithTemp [] = []
                    refill2DimArrayWithTemp (x : xs) = refillArrayWithTemp x : refill2DimArrayWithTemp xs -- 脱一层外壳
                      where
                        refillArrayWithTemp [] = []
                        refillArrayWithTemp (y : ys)
                            | y < 30 = 0 : refillArrayWithTemp ys
                            | y < 40 = 1 : refillArrayWithTemp ys -- 为测试方便，将1、2全部当成危险温度识别
                            | otherwise = 2 : refillArrayWithTemp ys
                let recogRectangle = tempRecogAlgo warnTempArray
                putStrLn $ "时间戳: " ++ show ts
                putStrLn $ "最高温度: " ++ show maxTemp ++ "°C"
                -- putStrLn $ "所有温度热力图: " ++ show tempColorMap
                putStrLn $ "警告温度: " ++ show recogRectangle
                putStrLn "--------------------------"
