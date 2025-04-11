fibonacci' :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fibonacci' x n m = if x == 0 then n else fibonacci' (x - 1) m (n + m)

fibonacci :: (Eq t1, Num t1, Num t2) => t1 -> t2
fibonacci x = fibonacci' x 0 1

fibonacciAlternative :: Int -> Int
fibonacciAlternative 0 = 0
fibonacciAlternative 1 = 1
fibonacciAlternative step = fibonacciAlternative (step - 1) + fibonacciAlternative (step - 2) -- too slow

fibonacciFull :: (Eq a, Num a, Num b, Enum a) => a -> [b]
fibonacciFull x = map fibonacci [1 .. x] -- fibonacci array

fibonacciFull2 :: [Integer]
fibonacciFull2 = [fibonacci x | x <- [0 ..]]

fibonacciAlternativeFull :: [Int]
fibonacciAlternativeFull = [fibonacciAlternative x | x <- [0 ..]]
