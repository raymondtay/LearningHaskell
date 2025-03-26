module FoldingWonders where

import Data.Char (digitToInt)

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc*10 + digitToInt x in loop acc' xs

asInt :: String -> Int
asInt xs = loop 0 xs

-- Key insight here is to understand that `foldl` consumes a step-function
-- , an initial value for its accumulator and a list.
-- Then, the step function takes an accumulator and an element from the list
-- and returns a new accumulator value.
asInt_fold :: String -> Int
asInt_fold "" = error "Invalid format"
asInt_fold (x:xs) | x == '-' = negate (foldl f 0 xs)
                  | otherwise = foldl f 0 (x:xs)
  where f acc b | '0' <= b && b <= '9' = acc * 10 + digitToInt b
              | otherwise = error ("Invalid character: " ++ [b])


