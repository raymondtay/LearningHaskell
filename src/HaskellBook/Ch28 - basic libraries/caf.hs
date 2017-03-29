module Main where

-- CAFs aka Constant Applicative Forms. CAFs are expressions that have no free
-- variables and are held in memory to be shared with all other expressions in
-- a module. They can be literal values or partially-applied functions, as long
-- as the arguments to the function aren't named arguments.
-- We are going to construct a very large CAF here.
--

-- According to Haskell's convention, this module **must** be named Main.
-- Then you do something like this:
-- > ghc -o caf -prof -fprof-auto -rtsopts ./caf.hs
-- > ./caf +RTS -p
-- > caf.prof is produced 
--


incdInts :: [Integer]
incdInts = map (+1) [1..]

main :: IO ()
main = do
  print (incdInts !! 1000)
  print (incdInts !! 9001)
  print (incdInts !! 90010)
  print (incdInts !! 900100)
  print (incdInts !! 9501000)
  print (incdInts !! 9901000)
  --print (incdInts' [1..] !! 1000)

-- Here's another way to avoid creating a CAF
--

incdInts' :: [Integer] -> [Integer]
incdInts' x = map (+1) x

-- place the following expression into main
-- print (incdInts' [1..] !! 1000)
--
