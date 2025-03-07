分数 score
  | 90 <= score && score <= 100 = "A"
  | 60 <= score && score <= 89 = "B"
  | 0 <= score && score <= 59 = "C"
  | otherwise = error "Invalid score!"