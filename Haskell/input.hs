import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
    -- 交互提示部分
    putStr "请输入一个数字（支持整数/小数）: "
    hFlush stdout -- 确保提示立即显示

    -- 带异常处理的输入
    input <- getLine
    case readMaybe input of
        Just num -> do
            let result = (num :: Double) ^ 2
            putStrLn $ "√ 计算结果: " ++ show result
        Nothing -> do
            putStrLn "× 输入错误！请确保输入的是数字"
            putStrLn "  有效示例: 3.14 或 42"
            exitFailure
