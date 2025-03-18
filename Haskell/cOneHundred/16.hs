最大公约数和最小公倍数 m n =
  if m > n
    then putStrLn $ "最大公约数:" ++ show (maxNum m n) ++ " 最小公倍数:" ++ show (minNum m n)
    else putStrLn $ "最大公约数:" ++ show (maxNum n m) ++ " 最小公倍数:" ++ show (minNum n m)
  where
    maxNum x y =
      if (x `mod` y == 0)
        then y
        else maxNum y (x `mod` y)
    minNum x y = x * y `div` (maxNum x y)
