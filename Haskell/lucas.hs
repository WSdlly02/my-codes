lucas' x n m = if x == 0 then n else lucas' (x-1) m (n+m)
lucas x = lucas' x 2 1
lucasArray = [ lucas x | x <- [0..]]
goldenRatio = map (\x -> lucasArray!!x / lucasArray!!(x+1)) [0..]