module Chap23 where

import Control.Monad.Trans.State

-- Inside System.Random, we see the following:
-- ****************************
-- Prelude System.Random> :i RandomGen
-- class RandomGen g where
--   next :: g -> (Int, g)
--   genRange :: g -> (Int, Int)
--   split :: g -> (g, g)
--   {-# MINIMAL next, split #-}
--   -- Defined in ‘System.Random’
--   instance RandomGen StdGen -- Defined in ‘System.Random’
--
-- class Random a where
--   randomR :: RandomGen g => (a, a) -> g -> (a, g)
--   random :: RandomGen g => g -> (a, g)
--   randomRs :: RandomGen g => (a, a) -> g -> [a]
--   randoms :: RandomGen g => g -> [a]
--   randomRIO :: (a, a) -> IO a
--   randomIO :: IO a
--   {-# MINIMAL randomR, random #-}
--   -- Defined in ‘System.Random’
-- *****************************


-- `Moi s a` is another way to write s -> (a, s)
-- and it can be confusing because we think there's
-- a special way to interpret `Moi s a`
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- this is tricky!
-- why so?
-- because my mind is not used to symbolic manipulation!
-- ARGHhhhhh
instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  -- breaking it down:
  -- g :: s -> (a, s)
  -- f :: a -> b
  fmap f (Moi g) = Moi $ \s -> 
    let (a, s1) = g s -- remember that g actually is s -> (a, s)
    in (f a, s1) 

--
-- Similarly as in the functor instance
--
instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let 
      (a, _) = g s -- "pull" the "a" out from the State
      (f', _) = f s -- "pull" the function out from the State
    in (f' a, s) -- ignore the derived states from the input states

--
-- Similarly as in Applicative and Functor, i applied
-- the same reasoning as highlighted before.
--
instance Monad (Moi s) where
  return = pure
  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> 
    let 
      (a, _) = f s
      (b, _) = runMoi (g a) $ s -- is it 's' or 's1' ?
    in (b, s)


--
-- i use `state` defined in the State Monad
-- for lifting the intended value "x" to be 
-- part of a State Monad.
--
get' :: State s s 
get' = state $ \x -> (x,x) 

-- 
-- construct a state where the resulting state is the argument
-- provided and the value is defaulted to unit
--
put' :: s -> State s ()
put' s = state $ \_ -> ((), s)

--
-- Run the state with S and get the state that results
-- In here, 'm' is the State object (i.e. s -> (a, s)) 
-- and 's' is the state in question.
--
exec' :: State s a -> s -> s
exec' m s = snd (runState m s)

--
-- Run the state with S and get the value that results
-- In here, 'm' is the State object (i.e. s -> (a, s)) 
-- and 's' is the state in question.
--
eval' :: State s a -> s -> a
eval' m s = fst (runState m s) 

-- 
-- This could be trickier than the others
-- as (s -> s) is a function which consumes a state
-- and returns a state. An endo function, that is.
-- When you follow the types, you will realize that 
-- actually modify'' needs to consume a function (which i assume
-- to be of type s -> s ).
--
modify'' :: (s -> s) -> State s ()
modify'' f = state $ \s -> ((), f s) 

{-
 - Here's an excerpt of how they would look like when you run them 
 - on the console terminal. Remember that State is a Monad 
 - *Chap23 Control.Monad.State System.Random> runState (modify'' (+1)) 9
 - ((),10)
 - *Chap23 Control.Monad.State System.Random> runState (modify'' (+1)) 0
 - ((),1)
 - *Chap23 Control.Monad.State System.Random> runState (modify'' (+1) >> modify'' (+1)) 0
 - ((),2)
 - *Chap23 Control.Monad.State System.Random>
 -
 -}
