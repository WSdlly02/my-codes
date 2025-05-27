binarySearch :: (Ord a) => a -> [a] -> Maybe Int
binarySearch target xs = search 0 (length xs - 1)
 where
  search low high
    | low > high = Nothing -- 搜索区间无效
    | otherwise =
        let mid = low + (high - low) `div` 2 -- 防溢出计算中间索引
            midVal = xs !! mid
         in case compare target midVal of
              EQ -> Just mid -- 找到目标
              LT -> search low (mid - 1) -- 左半区
              GT -> search (mid + 1) high -- 右半区