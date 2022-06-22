-- ❯ ghc --run ./basic_criterion.hs
-- macro 'doc' overwrites builtin command.  Use ':def!' to overwrite.
-- benchmarking index list 9999
-- time                 21.07 μs   (20.96 μs .. 21.23 μs)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 21.08 μs   (21.01 μs .. 21.22 μs)
-- std dev              324.6 ns   (205.7 ns .. 487.9 ns)
-- variance introduced by outliers: 11% (moderately inflated)
--
-- benchmarking index list maybe index 9999
-- time                 8.703 ms   (8.482 ms .. 9.242 ms)
--                      0.990 R²   (0.974 R² .. 1.000 R²)
-- mean                 8.565 ms   (8.499 ms .. 8.852 ms)
-- std dev              323.5 μs   (50.27 μs .. 707.9 μs)
-- variance introduced by outliers: 15% (moderately inflated)

module BasicCriterion where

import           Criterion.Main
import           Debug.Trace

infixl 9 !?


_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

myList :: [Int]
myList  = [1..9999]

main :: IO ()
main = defaultMain
  [
    bench "index list 9999" $ whnf (myList !!) 9998,
    bench "index list maybe index 9999" $ whnf (myList !?) 9998
  ]

