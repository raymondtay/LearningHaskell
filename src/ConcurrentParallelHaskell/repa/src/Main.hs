
module Main where

import Data.Array.Repa as Repa
import Tutorial1

main :: IO ()
main = do
  let newShape = (Z :. 5 :. 3) :: DIM2
  putStrLn . show . toList $ reify2D 3 5 -- canonical way to print an 2D array
  putStrLn . show . toList . reshape $ construct2D 3 5

