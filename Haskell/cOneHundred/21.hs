import Data.Time.Clock

main :: IO ()
main = do
    start <- getCurrentTime
    let result = 总桃子数
    end <- getCurrentTime
    print result
    putStrLn $ "耗时: " ++ show (diffUTCTime end start)

总桃子数 = last (take 10 (iterate (\x -> (x+1)*2) 1))