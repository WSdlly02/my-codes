import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  let result = 水仙花数
  end <- getCurrentTime
  print result
  putStrLn $ "耗时: " ++ show (diffUTCTime end start)

水仙花数 = 水仙花数 [0 .. 9] [0 .. 9] [0 .. 9]
  where
    水仙花数 xs ys zs = [x * 100 + y * 10 + z | x <- xs, y <- ys, z <- zs, (x * 100 + y * 10 + z) == (x ** 3 + y ** 3 + z ** 3), x /= 0]
