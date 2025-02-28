fibonacci' x n m = if x == 0 then n else fibonacci' (x-1) m (n+m)
fibonacci x = fibonacci' x 0 1
