huiwen :: [Integer]
huiwen = huiwen [1 .. 9] [0 .. 9] [0 .. 9] [0 .. 9] [0 .. 9]
 where
  huiwen as bs cs ds es = [a * 10000 + b * 1000 + c * 100 + d * 10 + e | a <- as, b <- bs, c <- cs, d <- ds, e <- es, a == e, b == d]
