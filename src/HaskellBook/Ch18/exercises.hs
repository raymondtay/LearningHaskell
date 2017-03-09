{-# LANGUAGE InstanceSigs #-}

module Chapter_18 where

import Control.Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure :: b -> Sum a b
  pure a = Second a
  (<*>) :: (Sum a) (b -> c) -> (Sum a) b -> (Sum a) c
  (<*>) (First f) (First a) = First a
  (<*>) (First f) (Second a) = First f
  (<*>) (Second f) (First b) = First b
  (<*>) (Second f) (Second a) = Second (f a)

--
-- The explanations for the Functors and Applicatives were given in Chapter 17
-- and possibly Chapter 16 but the prior mention has the most detailed
-- explanations. For this, we need to dissect it a little.
--
-- return is the same as pure ... that should be obvious 
--
-- To understand the `bind` operator, its important to understand the following
-- point and that is the fact that `Sum a` is bound to the definition of Monad
-- which means that the data constructor First a cannot be transformed via the
-- function 'f'.
--
instance Monad (Sum a) where
  return = pure
  (>>=) :: (Sum a) b -> (b -> Sum a c) -> Sum a c
  (First a) >>= f = First a
  (Second a) >>= f = f a
  

