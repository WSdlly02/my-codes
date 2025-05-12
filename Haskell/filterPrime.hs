import System.IO (hFlush, stdout)

primes :: (Integral a) => a -> [a]
primes x = filterPrime [2 .. x]
 where
  filterPrime [] = []
  filterPrime (p : xs) =
    p : filterPrime [x | x <- xs, x `mod` p /= 0]

filterTriangle :: (Ord a, Num a) => [a] -> [a]
filterTriangle [_] = []
filterTriangle [_, _] = []
filterTriangle [_, _, _] = []
filterTriangle (x1 : x2 : x3 : xs) =
  if x1 + x2 > x3 && x1 + x3 > x2 && x2 + x3 > x1
    then x1 : x2 : x3 : filterTriangle xs
    else filterTriangle xs

main :: IO ()
main = do
  putStr "请输入一个数字(支持整数/小数):"
  input <- getLine
  let num = read input :: Integer
  print $ filterTriangle (primes num)
