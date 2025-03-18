import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  let lengthArray = concatMap (replicate 2) (take 11 (iterate (\x -> x / 2) 100))
      totalLength = sum lengthArray - (last lengthArray) * 2 - head lengthArray
  end <- getCurrentTime
  putStrLn $ "总路程:" ++ show totalLength ++ " 第10次路程:" ++ show (last lengthArray)
  putStrLn $ "耗时: " ++ show (diffUTCTime end start)
