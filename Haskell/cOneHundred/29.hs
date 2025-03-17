import System.IO
import System.Exit (exitFailure)
import Text.Read (readMaybe)

numFigures x = step x 0 where
  step x n = if x `mod` (map (\x -> 10^x) [0..] !! n) == x
    then n
    else step x (n+1)

main :: IO ()
main = do
  -- 交互提示部分
  putStr "请输入一个整数:\n"
  hFlush stdout  -- 确保提示立即显示
  
  input <- getLine
  case readMaybe input of
    Just n -> do
      let reversedInput = read (reverse input) :: Int -- 将输入的字符串颠倒再转为整数
      let result = numFigures (n :: Int) -- 声明n为Int
      putStrLn $ "输入的数字为" ++ show result ++ "位数,逆序为:" ++ show reversedInput
    Nothing -> do
      putStrLn "× 输入错误！请确保输入的是数字"
      putStrLn "  有效示例:42"
      exitFailure
