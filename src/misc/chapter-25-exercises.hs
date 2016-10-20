{-# LANGUAGE InstanceSigs #-}

module Chap25 where

import Data.Foldable

--
-- `{-# LANGUAGE InstanceSigs #-}` 
-- Allows type signatures to be specified in 
-- instance declarations.
--

-- 
-- Over here, this represents a type constructors, not term-level expressions
--
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- 
-- Sample usage: 
-- (\x -> x + 1) <$> Compose [Just 1]
-- Compose { getCompose = [Just 2] }
-- 
-- From the above, we can see that (\x -> x + 1) is lifted twice into `Compose`
-- to apply the lambda to the value of '2' (parlance would be `..mapped over the value..`)
-- inside the `Maybe` and doesn't alter
-- the structure at all; hence we see its `Compose { getCompose = [Just 2]}`.
--
--
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure -- why is 'pure' used? because the declaration already said that f ∈ Applicative, g ∈ Applicative.
                               -- and also type of `pure . pure :: (Applicative f, Applicative f1) => a -> f (f1 a)`
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f') <*> (Compose a') = Compose $ ((<*>) <$> f') <*> a'


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- 
-- Here's how a multi-layered functor can be potentially 
-- expressed.
--
newtype One f a = One (f a) deriving (Eq, Show)
instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)
instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

--
-- An example using `Compose` would be the following:
--
-- *Chap25 System.Random Control.Applicative> let v = Compose [Just (Compose $ Just [1])]
-- *Chap25 System.Random Control.Applicative> :t v
-- v :: Num a => Compose [] Maybe (Compose Maybe [] a)
--
--

-- 
-- It's a functor that can map over two type arguments instead of just one.
--
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id


data Deux a b = Deux a b deriving (Show)

instance Bifunctor Deux where
  bimap f g = first f . second g

data Const a b = Const a
instance Bifunctor Const where
  first f = bimap f id
  second = bimap id

data Drei a b c = Drei a b c 
instance Bifunctor (Drei a) where
  first f = bimap f id
  second = bimap id

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  first f = bimap f id
  second = bimap id

data SemiDrei a b c = SemiDrei a 
instance Bifunctor (SemiDrei a) where
  first f = bimap f id 
  second = bimap id

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
  first f = bimap f id
  second = bimap id

-- See the README-chap-25.md for a detailed explanation of how
-- this works.
monads_1 = [Right 4] >>= (\x -> [Right (+4) <*> x])
monads_2 = [[Right 4]] >>= (\x -> x >>= (\y -> [Right (+4) <*> y]))

-- 
-- Understanding Monad Transformers
-- 
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a )

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f


innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)
