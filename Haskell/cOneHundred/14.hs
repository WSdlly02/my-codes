import Data.List (intercalate)

分解质因数 n = do
    let factors = primeFactors n
    putStrLn $ formatResult n factors

-- 质因数分解核心算法
primeFactors :: Int -> [Int]
primeFactors 1 = [1]
primeFactors n = factorize n 2
  where
    factorize 1 _ = []
    factorize n d
        | d * d > n = [n] -- 剩余数为质数
        | n `mod` d == 0 = d : factorize (n `div` d) d
        | otherwise = factorize n (d + 1)

-- 格式化输出结果
formatResult :: Int -> [Int] -> String
formatResult n factors
    | n == 1 = "1 = 1"
    | otherwise = show n ++ " = " ++ intercalate "*" (map show factors)
