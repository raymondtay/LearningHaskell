
module Tutorial1 () where

import Data.Array.Repa as Repa

--
-- General idea is to start by familiarising with the functions of the Repa
-- package and its likely i will start by array creation and then extend it to
-- manipulating the created arrays and finally see how to manifest these
-- arrays.
--

--
-- 1. Construct a 2-D array
--
construct2D :: Int -> Int -> Array D DIM2 Int
construct2D r c =
  let my2D = (Z :. r :. c) :: DIM2
  in fromFunction my2D (\(Z:.x:.y) -> x*c + y)

--
-- As a start, there's no obvious need to invoke the parallel creation of a 2-D
-- array. If the dimensions are pretty large, there could be a need but for
-- tutorial purposes, i'm excluding it.
--
reify2D :: Int -> Int -> Array U DIM2 Int
reify2D r c = computeS $ construct2D r c 


