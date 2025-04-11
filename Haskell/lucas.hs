lucas' :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
lucas' x n m = if x == 0 then n else lucas' (x - 1) m (n + m)
lucas :: (Eq t1, Num t1, Num t2) => t1 -> t2
lucas x = lucas' x 2 1
lucasArray :: [Double]
lucasArray = [lucas x | x <- [0 ..]]
goldenRatio :: [Double]
goldenRatio = map (\x -> lucasArray !! x / lucasArray !! (x + 1)) [0 ..]