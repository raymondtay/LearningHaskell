import Control.Monad.Par
import Control.DeepSeq

par_filter :: NFData a => (a -> Bool) -> [a] -> Par [a]
par_filter f [] = return []
par_filter f (x:[]) = if f x then return [x] else return []
par_filter f xs = do
  let (as, bs) = halve xs
  x1 <- spawn $ par_filter f as
  x2 <- spawn $ par_filter f bs
  left <- get x1
  right <- get x2
  return $ left ++ right
    where
      halve :: [a] -> ([a], [a])
      halve datum = splitAt (length datum `div` 2) datum

-- Two tasks are spawned to filter the two parts of the list which is run in
-- parallel eventually. The results are concatenated and returned to the
-- caller.

-- An expensive function to operate on
f :: (Num a, Ord a) => a -> Bool
f x = nfib 5 `deepseq` x < 10
  where 
    nfib :: Integer -> Integer
    nfib n | n < 2 = 1
    nfib n = nfib (n-1) + nfib (n-2)

{-
  | The above function "par_filter" is actually not proper use of the Par Monad for the reason that
  | a lot of parallel tasks are being form till the point where there is 1 parallel task that operates
  | on exactly 1 data element. Awful waste of compute power and the key idea is to make each parallel task
  | operate against more data elements.
  |
-}

seq_filter :: (a -> Bool) -> [a] -> [a]
seq_filter _ [] = []
seq_filter f (x:xs)
  | f x = x : seq_filter f xs
  | otherwise = seq_filter f xs

par_filter2 :: NFData a => Int -> (a -> Bool) -> [a] -> Par[a]
par_filter2 n f xs = (<$>) concat $ parMap (seq_filter f) $ chunk n xs

chunk :: Int -> [a] -> [[a]]
chunk n xs = as : chunk n bs
  where (as, bs) = splitAt n xs

