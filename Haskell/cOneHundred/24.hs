fibonacci' :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fibonacci' x n m = if x == 0 then n else fibonacci' (x - 1) m (n + m)

fibonacci :: (Eq t1, Num t1, Num t2) => t1 -> t2
fibonacci x = fibonacci' x 0 1

fibonacciDivSum :: Double
fibonacciDivSum = sum (map (\n -> fibonacci n / fibonacci (n - 1)) [3 .. 22])
