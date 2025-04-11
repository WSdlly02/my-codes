factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n - 1)

accumMulti :: [Integer]
accumMulti = map factorial [1 .. 20]
