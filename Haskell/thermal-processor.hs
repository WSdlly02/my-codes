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
                let maxTemp = maximum temps
                -- let warnTempArray = filter (>30) temps
                putStrLn $ "时间戳: " ++ show ts
                putStrLn $ "最高温度: " ++ show maxTemp ++ "°C"
                putStrLn $ "所有温度: " ++ show temps
                putStrLn "--------------------------"