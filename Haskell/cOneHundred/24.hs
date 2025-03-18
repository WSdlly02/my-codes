fibonacci' x n m = if x == 0 then n else fibonacci' (x - 1) m (n + m)

fibonacci x = fibonacci' x 0 1

fibonacciDivSum = sum (map (\n -> fibonacci n / fibonacci (n - 1)) [3 .. 22])
