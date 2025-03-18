import System.IO

main :: IO ()
main = do
    -- 交互提示部分
    putStr "请输入一串字符:"
    hFlush stdout -- 确保提示立即显示
    input <- getLine
    let output = reverse input
    putStrLn $ show output
