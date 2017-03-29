-- Criterion module walkthrough.
--
module Criterion where

-- BOS wrote a tutorial which can be found here 
-- http://www.serpentine.com/criterion/tutorial.html
--
import Debug.Trace
import Criterion.Main

-- declares a new operator `!?` which is left-associative and has a precedence level of 9.
infixl 9 !?

_      !? n | n < 0 = Nothing
[]     !? _ = Nothing
(x: _) !? 0 = Just x
(_:xs) !? n = xs !? (n-1)

myList :: [Int]
myList = trace "myList was evaluated" ([1..9999] ++ [undefined])

main :: IO ()
main = defaultMain
  [ bench "index list 9999" 
    $ whnf (myList !!) 9998,
    bench "index list maybe index 9999"
    $ whnf (myList !!) 9998]

