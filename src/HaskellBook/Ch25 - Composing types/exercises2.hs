{-# LANGUAGE InstanceSigs #-}


module Ch25_exercises2 where

import Prelude hiding (Either, Left, Right) -- hid these definitions to prevent interferring with the last definition in this file.

-- See Page 993 for details.
--
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  first :: (a -> b) -> Deux a c -> Deux b c
  first f (Deux a c)= Deux (f a) c

  second :: (b -> c) -> Deux a b -> Deux a c
  second f (Deux a b) = Deux a (f b)

data Const a b = Const a
instance Bifunctor Const where
  first f (Const a) = Const (f a)
  second f (Const a) = Const a

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  first f (Drei a b c) = Drei a (f b) c
  second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where -- remember that type parameter 'a' is bound to SuperDrei which is inturn bound to Bifunctor
  first f (SuperDrei a b) = SuperDrei a (f b)
  second f (SuperDrei a b) = SuperDrei a b

data Either a b = Left a | Right b
instance Bifunctor Either where
  first f (Left a) = Left (f a)
  second f (Right b) = Right (f b)


