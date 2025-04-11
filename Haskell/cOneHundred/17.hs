import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStr "请输入一句话:"
    hFlush stdout -- 确保提示立即显示
    input <- getLine
    let letter = length (filter (`elem` ['a' .. 'z'] ++ ['A' .. 'Z']) input) -- filter isAlpha
        number = length (filter (`elem` ['0' .. '9']) input)
        space = length (filter (== ' ') input)
        otherChar = length input - letter - space - number
    putStr $ "字母:" ++ show letter ++ " 数字:" ++ show number ++ " 空格:" ++ show space ++ " 其他:" ++ show otherChar ++ "\n"
