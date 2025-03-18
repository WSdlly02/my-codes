import Data.List (nub, (\\))
import Data.Time.Clock

main :: IO ()
main = do
    start <- getCurrentTime
    let result = perfectNumbers
    end <- getCurrentTime
    print result
    putStrLn $ "耗时: " ++ show (diffUTCTime end start)
  where
    perfectNumbers = filter isPerfect [1 .. 1000]
    isPerfect n = sum (factors n) == n
    factors n =
        concat
            [ if x == y then [x] else [x, y]
            | x <- [1 .. m]
            , n `mod` x == 0
            , let y = n `div` x
            ]
            \\ [n] -- 排除数字本身
      where
        m = floor (sqrt (fromIntegral n))

-- 运行结果: [6, 28, 496]
