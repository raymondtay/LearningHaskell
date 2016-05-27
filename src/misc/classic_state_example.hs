module IncrementCounter where

import Control.Monad.State.Lazy

{-
 - In the classic example of a State Monad found here
 - http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html
 - i attempt to dissect the example given in that same page 
 - in my pursuit to understand what actually happens.
 -
 - Let's begin.
 -
 - So in the beginning of the page, there's a mention of a MonadState
 - which encapsulates the 3 ideas of a state
 - (a) get - returns the current state
 - (b) put - replace the state
 - (c) state - which embeds a function (i.e. action) into the state
 - The function "tick" below illustrates the basic idea.
 - 
 - To run it, load this into your ghci via commandline or via stack
 - and i'll discuss about runState shortly below.
 - $> runState tick 1
 - (1,2)
 - $> runState tick 2
 - (2,3)
 - $> runState tick 3
 - (3,4)
 -}

tick :: State Int Int
tick = do
  n <- get
  put (n+1)
  return n

--
-- Add one to the given number using the state monad
--
plusOne :: Int -> Int
plusOne n = execState tick n



