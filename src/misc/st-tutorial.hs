module TutorialSTMonad where

import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import System.Random

{-
The Control.Monad.ST library provides support for
strict state threads, as described in PLDI'94
paper by John and Simon Lazy Functional State Threads.

References (variables) that can be used within the ST monad 
are provided by Data.STRef, and arrays are provided by Data.Array.ST

-}

-- 
-- basically says that when we call "runState" by passing it 
-- the current state "s" we get a pair of values where 
-- "a" represents the output value and the new state "s" is embedded
-- in the 2nd element of the pair
--
newtype State s a = State { runState :: s -> (a, s) }

type Iso a b = (a -> b , b -> a)
newtype Sum a = Sum { getSum :: a}
sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)























