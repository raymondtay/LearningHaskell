{-
 - ❯ ghc --run -prof -fprof-auto -rtsopts -O2 ./map_set_comparison.hs
 -
 - when making flags consistent: warning:
 -     -O conflicts with --interactive; -O ignored.
 - macro 'doc' overwrites builtin command.  Use ':def!' to overwrite.
 - benchmarking member check map
 - time                 195.5 ns   (192.2 ns .. 199.4 ns)
 -                      0.999 R²   (0.997 R² .. 1.000 R²)
 - mean                 192.4 ns   (191.3 ns .. 194.3 ns)
 - std dev              4.855 ns   (2.553 ns .. 7.548 ns)
 - variance introduced by outliers: 36% (moderately inflated)
 -
 - benchmarking member check set
 - time                 231.4 ns   (225.6 ns .. 239.6 ns)
 -                      0.996 R²   (0.991 R² .. 1.000 R²)
 - mean                 225.6 ns   (223.6 ns .. 231.0 ns)
 - std dev              10.06 ns   (5.326 ns .. 18.62 ns)
 - variance introduced by outliers: 64% (severely inflated)
-}

module Main where

import           Criterion.Main

import qualified Data.Map       as M
import qualified Data.Set       as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream where stream = iterate (+1) 0


membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

main :: IO ()
main = defaultMain
  [ bench "member check map" $ whnf membersMap 9999,
    bench "member check set" $ whnf membersSet 9999
  ]


