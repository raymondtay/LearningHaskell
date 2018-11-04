
-- Here's what a filter functio n applied sequentially would look like.
-- See [[par_filter.hs]] for more details.
seq_filter :: (a -> Bool) -> [a] -> [a]
seq_filter _ [] = []
seq_filter f (x:xs)
  | f x = x : seq_filter f xs
  | otherwise = seq_filter f xs

