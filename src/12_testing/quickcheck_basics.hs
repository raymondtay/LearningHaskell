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
