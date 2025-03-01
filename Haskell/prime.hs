primes = filterPrime [2..] where
  filterPrime (p:xs) =
    p : filterPrime [x | x <- xs, x `mod` p /= 0]

{-
1.primes = filterPrime [2,3,4,5,6,7,8,...]
--> 2 : filterPrime [3,5,7,9,...]  -- 筛除所有 2 的倍数（4,6,8...）
2.filterPrime [3,5,7,9,11,13,...]
--> 3 : filterPrime [5,7,11,13,...]  -- 筛除所有 3 的倍数（9,15...）
3.filterPrime [5,7,11,13,17,...]
--> 5 : filterPrime [7,11,13,17,...]  -- 筛除所有 5 的倍数（25,35...）
-}
