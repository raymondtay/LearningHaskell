
module AboutStrategies where

import Control.Parallel
import Control.Parallel.Strategies

-- Run: runEval $ simple (1, 2) OR equivalently in applicative-style:
-- (,) <$> rpar 1 <*> rpar 2
--
-- Run: runEval $ simple ([undefined], 2)
-- should vomit the callstack since Eval is a strict Monad and again in
-- applicative-style:
-- (,) <$> rpar [undefined] <*> rpar 2
--
simple :: Strategy (a, b)
simple (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

-- Run: runEval $ composition (1,2)
composition = simple `dot` simple -- equivalent to simple `dot` simple `dot` r0; r0 is a no-op.

fib :: Integer -> Integer
fib n | n == 1 = 1
  | n == 0 = 0
  | otherwise = fib (n-1) + fib (n-2)

efib :: Integer -> Integer
efib n | n == 1 = 1
  | n == 0 = 0
  | otherwise = runEval $ do
    a <- rpar (efib (n-1))
    b <- rpar (efib (n-2))
    return (a + b)

-- Strategy version. The interesting observation here is that "using" is
-- defined as " a -> Strategy a -> a".
ffib :: Integer -> Integer
ffib n | n == 0 = 0
  | n  == 1 = 1
  | otherwise = using (a + b) strat
  where a = fib (n - 1)
        b = fib (n - 2)
        strat v = do rpar a; rseq b; return v

scanP :: NFData a => Int -> (a -> a -> a) -> [a] -> [a]
scanP d f list = concat reducedList
  where
    reducedList = reduce f scanList
    scanList = map (scanl1 f) (chunk d list) `using` parList rdeepseq

reduce f [] = []
reduce f [x] = [x]
reduce f (x:(y:xs)) = x : reduce f (map (f $ last x) y : xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs
  where (as, bs) = splitAt n xs

