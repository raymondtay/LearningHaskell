module MyQuickCheck where

import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs = filter (< x ) xs
        rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

-- To test the above function `prop_idempotent`, we could launch it in
-- ghci like this:
-- quickCheck (prop_idempotent :: [Integer] -> Bool)
-- +++ OK, passed 100 tests.
-- it :: ()
-- (0.04 secs, 13966312 bytes)

-- quickCheck (prop_idempotent :: [Integer] -> Bool)

--
-- Another function 
--
mhead :: [a] -> a
mhead (x:_) = x
mhead []    = error "Empty list. Oops"

prop_mhead (x:xs) = mhead (x:xs) == x

-- quickCheck (prop_mhead :: [Integer] -> Bool)

xminimum :: (Ord a) => [a] -> a
xminimum [] = error "Empty list. Oops"
xminimum xs = foldl1 min xs

prop_xminimum xs = not (null xs) ==> head (qsort xs) == minimum xs
-- quickCheck prop_xminimum

prop_ordered xs = ordered (qsort xs)
  where ordered [] = True
        ordered [x] = True
        ordered (x:y:ys) = x <= y && ordered (y:xs)
-- quickCheck (prop_ordered:: [Integer] -> Bool)

prop_permutation xs = permutation xs (qsort xs)
  where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)
-- quickCheck (prop_permutation:: [Integer] -> Bool)

prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_append xs ys = not (null xs)  && not (null ys) ==> head (qsort (xs ++ ys)) == min (xminimum xs) (xminimum ys)

