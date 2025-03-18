import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs -- 获取命令行参数列表
  case args of
    [] -> putStrLn "错误：未提供参数" -- 无参数时的处理
    (x : _) -> putStrLn $ "第一个参数是: " ++ x -- 提取并打印第一个参数
