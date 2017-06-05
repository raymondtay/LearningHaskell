module Main where

incdInts :: a -> [Integer]
incdInts _ = map (+1) [1..]

-- You know why your code needs to be point-free style? Turns out there's a
-- REAL practical reason and that is point-free top-level declarations will be
-- CAFs but pointful ones are not.
--

main :: IO ()
main = do
  print (incdInts 0 !! 1000)
  print (incdInts 0 !! 9001)
  print (incdInts 0 !! 90010)
  print (incdInts 0 !! 9001000)
  print (incdInts 0 !! 9501000)
  print (incdInts 0 !! 9901000)

