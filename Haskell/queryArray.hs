queryArray :: (Eq t1, Num t1) => [t2] -> t1 -> t2
queryArray (x : xs) step =
  if step == 0
    then x
    else queryArray xs (step - 1)