{-
 - ❯ ghc --run -prof -fprof-auto -rtsopts -O2 ./vector_list_comparison.hs
 -
 - when making flags consistent: warning:
 -     -O conflicts with --interactive; -O ignored.
 - macro 'doc' overwrites builtin command.  Use ':def!' to overwrite.
 - benchmarking slicing list
 - time                 689.0 ns   (688.4 ns .. 689.7 ns)
 -                      1.000 R²   (1.000 R² .. 1.000 R²)
 - mean                 688.2 ns   (687.1 ns .. 689.1 ns)
 - std dev              3.268 ns   (2.709 ns .. 4.278 ns)
 -
 - benchmarking slicing vector
 - time                 41.68 ns   (41.40 ns .. 41.89 ns)
 -                      1.000 R²   (1.000 R² .. 1.000 R²)
 - mean                 41.30 ns   (41.15 ns .. 41.65 ns)
 - std dev              768.6 ps   (457.5 ps .. 1.345 ns)
 - variance introduced by outliers: 26% (moderately inflated)
-}

module Main where

import           Criterion.Main
import qualified Data.Vector    as V

slice :: Int -> Int -> [a] -> [a]
slice from to xs =
  take (to - from + 1) (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

main :: IO ()
main = defaultMain
  [ bench "slicing list" $ whnf (head . slice 100 900) l,
    bench "slicing vector" $ whnf (V.head . V.slice 100 900) v
  ]

