{-# LANGUAGE BangPatterns #-}

import Data.Functor.Identity
import Data.Array.Repa as Repa
import System.Environment (getArgs)

type Weight = Int
type Graph r = Array r DIM2 Weight

-- This algorithm makes an assumption that the graph should be connected
-- otherwise it would run into an infinite loop. How so? You only have to
-- examine the algorithm and its possible that can happen.
-- There is a data dependency between the k-th and (k+1)-th iteration when
-- a shortest path was detected.
-- https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
--
-- The original Floyd-Warshall algorithm
-- compares all possible paths through the graph between each pair of vertices.
-- hence its a deterministic algorithm that is guaranteed to run through
-- everything - question is whether.
-- It is able to do this with O(V^3) comparisons in a graph. This is remarkable
-- considering that there may be up to O(V^2) edges in the graph  and every
-- combination of edges is tested.
--
shortestPaths :: Graph U -> Graph U
shortestPaths g0 = go g0 0
  where
    Z :. _ :. n = extent g0
    go !g !k
      | k == n = g
      | otherwise =
        let g' = computeS (fromFunction (Z :. n :. n) sp)
        in go g' (k+1)
        where 
          sp (Z :. i:. j) = min (g ! (Z:.i:.j)) (g ! (Z:.i:.k) + g ! (Z:.k:.j))

-- `computeP` runs the computation in parallel (in each cpu core) and runs
-- in a Monad.
shortestPathsP :: Graph U -> Graph U
shortestPathsP g0 = runIdentity $ go g0 0
  where
    Z :. _ :. n = extent g0
    go !g !k
      | k == n = return g
      | otherwise = do
          g' <- computeP (fromFunction (Z :. n :. n) sp)
          go g' (k+1)
        where 
          sp (Z :. i:. j) = min (g ! (Z:.i:.j)) (g ! (Z:.i:.k) + g ! (Z:.k:.j))

maxDistance :: Weight -> Weight -> Weight
maxDistance x y
  | x == inf  = y
  | y == inf  = x
  | otherwise = max x y

maxDistances :: Graph U -> Array U DIM1 Weight
maxDistances = foldS maxDistance inf

inf :: Weight
inf = 999

testGraph :: Graph U
testGraph = toAdjMatrix $
        [[  0, inf, inf,  13, inf, inf],
         [inf,   0, inf, inf,   4,   9],
         [ 11, inf,   0, inf, inf, inf],
         [inf,   3, inf,   0, inf,   7],
         [ 15,   5, inf,   1,   0, inf],
         [ 11, inf, inf,  14, inf,   0]]

-- correct result:
expectedResult :: Graph U
expectedResult = toAdjMatrix $
         [[0,  16, inf, 13, 20, 20],
          [19,  0, inf,  5,  4,  9],
          [11, 27,   0, 24, 31, 31],
          [18,  3, inf,  0,  7,  7],
          [15,  4, inf,  1,  0,  8],
          [11, 17, inf, 14, 21,  0] ]

test :: Bool
test = shortestPaths testGraph == expectedResult

toAdjMatrix :: [[Weight]] -> Graph U
toAdjMatrix xs = fromListUnboxed (Z :. k :. k) (concat xs)
  where k = length xs


main :: IO ()
main = do
   [n] <- fmap (fmap read) getArgs
   let g = fromListUnboxed (Z:.n:.n) [0..n^(2::Int)-1] :: Graph U
   print (sumAllS (shortestPaths g))


