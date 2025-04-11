year :: Integer
year = year 10
 where
  year n = last (take 5 [n, n + 2 ..])
