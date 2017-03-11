{-# LANGUAGE InstanceSigs #-}

module Chapter_23 where

import Control.Monad (join)

-- If i look back at Chapter 21,22 and earlier i can discover that i know what
-- these symbols mean now and its considerably easier to build functions. 
--
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- Functor instance for Moi
-- It really becomes clearer once i understand what those symbols mean.
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s -> let (a, s2) = (g s) in ((f a), s2)
                   
-- Applicative instance for Moi
-- It really becomes clearer once i understand what those symbols mean.
--
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = 
    Moi $ \s ->
      let (ff, s2) = (f s)
          (a, s3) = (g s) in ((ff a), s3)

-- Monad instance for Moi
-- The easiest thing to get confuse is when we are dealing with symbols and its
-- symbolic interpretation.
--
instance Monad (Moi s) where
  return :: a -> Moi s a
  return a = Moi (\s -> (a,s))

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi (\s ->
      let (a, s2) = (f s)
          (b, s3) = runMoi (g a) $ s
      in (b, s))

