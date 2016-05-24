module Chap23 where

import Control.Monad.Trans.State

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
      (a, g1) = g s -- "pull" the "a" out from the State
      (f', _) = f s -- "pull" the function out from the State
    in (f' a, g1)

--
-- Similarly as in Applicative and Functor, i applied
-- the same reasoning as highlighted before.
--
instance Monad (Moi s) where
  return = pure
  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> 
    let 
      (a, s1) = f s
      (b, s2) = runMoi (g a) $ s -- i have to use `runMoi` on (g a)
    in (b, s2)


--
-- i use `state` defined in the State Monad
-- for lifting the intended value "x" to be 
-- part of a State Monad.
--
get' :: State s s 
get' = state $ \x -> (x,x) 

