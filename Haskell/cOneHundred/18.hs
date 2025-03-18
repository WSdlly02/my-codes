连续递增加法 m n = sum (take n (iterate (\x -> x * 10 + m) m)) -- Best

连续递增加法' m n = sum (scanl1 (+) (map (\x -> m * (10 ^ x)) [0 .. n - 1]))
