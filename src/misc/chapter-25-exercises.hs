{-# LANGUAGE InstanceSigs #-}

module Chap25 where

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




