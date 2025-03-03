fibonacci' x n m = if x == 0 then n else fibonacci' (x-1) m (n+m)
fibonacci x = fibonacci' x 0 1
fibonacciAlternative :: Int -> Int
fibonacciAlternative 0 = 0
fibonacciAlternative 1 = 1
fibonacciAlternative step = fibonacciAlternative (step-1) + fibonacciAlternative (step-2) -- too slow
fibonacciFull x = map fibonacci [1..x] -- fibonacci array
fibonacciFull2 = [ fibonacci x | x <- [0..]]
fibonacciAlternativeFull = [fibonacciAlternative x | x <- [0..]]