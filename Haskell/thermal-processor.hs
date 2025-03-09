-- ThermalProcessorBinary.hs
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever, replicateM)  -- 添加此导入
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO (hSetBuffering, BufferMode(..), stdin)

-- 数据解析函数保持不变
parseThermalData :: Get (Double, [Float])
parseThermalData = do
    timestamp <- getDoublele
    temperatures <- replicateM 768 getFloatle  -- 现在 replicateM 已可用
    return (timestamp, temperatures)

-- 主程序
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    let chunkSize = 8 + 4 * 768

    forever $ do  -- 现在 forever 已可用
        bytes <- B.hGet stdin chunkSize
        case runGetOrFail parseThermalData (BL.fromStrict bytes) of
            Left (_, _, err) ->
                putStrLn $ "解析错误: " ++ err
            Right (_, _, (ts, temps)) -> do
                let maxTemp = maximum temps -- 求最大温度
                let tempArray = array2dim temps where -- 将768个温度数据存进32*24的二维矩阵
                    array2dim [] = []
                    array2dim (xs) = take 32 (xs) : array2dim (drop 32 xs)
                let warnTempArray = refill2DimArrayWithTemp tempArray where -- 过滤矩阵，记录危险温度
                    refill2DimArrayWithTemp [] = []
                    refill2DimArrayWithTemp (x:xs) = refillArrayWithTemp x : refill2DimArrayWithTemp xs where
                        refillArrayWithTemp [] = []
                        refillArrayWithTemp (y:ys)
                            | y<30 = 0 : refillArrayWithTemp ys
                            | y<40 = 1 : refillArrayWithTemp ys
                            | otherwise = 2 : refillArrayWithTemp ys
                putStrLn $ "时间戳: " ++ show ts
                putStrLn $ "最高温度: " ++ show maxTemp ++ "°C"
                putStrLn $ "所有温度: " ++ show tempArray
                putStrLn $ "警告温度: 已省略" -- ++ warnTempArray
                putStrLn "--------------------------"
