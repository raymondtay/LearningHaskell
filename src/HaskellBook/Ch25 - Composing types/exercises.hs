{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

import Control.Monad (join)

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show) 
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show) 

instance Functor Identity where
 fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
 fmap f (IdentityT g) = IdentityT (fmap f g)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity
  
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity (f a) 

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return :: a -> Identity a
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Identity a) >>= f = f a
-- Let's dissect the monadic instance a little bit...
-- First we pattern match or unpack the 'm a' value of 'IdentityT m a' via the
-- data constructor. Doing this has the type 'IdentityT m a -> m a' ad the type
-- of 'ma' is 'm a'. This nomenclature doesn't mean anything beyond mnemonic
-- signaling, but it is intended to be helpful.
--
-- The type of the bind we are implementing is the following:
--
-- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
-- This is the instance we are defining.
--
-- This is the function we are binding over 'IdentityT m a'. It has the
-- following type: '(a -> IdentityT m b)'.
--
-- Here 'ma' is the same one we unpacked out of the IdentityT data constructor
-- and has the type 'm a'. Removed from its IdentityT context, this is now the
-- 'm a' that this bind takes as its first argument.
--
-- This is a different bind! The first bind is the bind we are trying to
-- implement; this bind is its definition or implementation. We are now using
-- the Monad we asked for in the instance declaration with the constraint
-- 'Monad m =>'. This will have the type:
--
-- (>>=) :: m a -> (a -> m b) -> m b
-- 
--
instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f


-- We need runIdentityT because f returns IdentityT m b, but the >>= for the
-- Monad m => has the type m a -> (a -> m b) -> m b. It will end up trying to
-- join m (IdentityT m b) , which won't work because m and IdentityT m are not
-- the same type. We use runIdentityT to unpack the value. Doing this has the
-- type IdentityT m b -> m b and the composition runIdentityT . f in tihs
-- context has the type a -> m b. You can use undefined in GHCi to demonstrate
-- this for yourself:
--
-- *Chapter25> let f :: (a -> IdentityT m b); f = undefined
-- *Chapter25> :t f
-- f :: a -> IdentityT m b
-- *Chapter25> :t runIdentityT
-- runIdentityT :: IdentityT f a -> f a
-- *Chapter25> :t (runIdentityT . f)
-- (runIdentityT . f) :: a1 -> f a
-- *Chapter25>
--
--
-- To satisfy the type of the outer bind, we are implementing for the Monad of
-- IdentityT m, which expects a final result of the type IdentityT m b, we must
-- take the m b which the expression ma >>= runIdentityT . f returns and repack
-- it in IdentityT. Note:
--


newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

-- This is quite straight-forward since it really means lifting 'f' twice into
-- the structure.
--
instance (Functor f, Functor g) =>  Functor (Compose f g) where
  fmap f (Compose g) = Compose $ (fmap . fmap) f g

-- Constructing the following example from the book required the following
-- hint:
-- *Chapter25 Control.Monad> :t ((<*>) <$>)
--   ((<*>) <$>)
--   :: (Functor f1, Applicative f) =>
--   f1 (f (a -> b)) -> f1 (f a -> f b)
--
-- This ((<*>) <$>) needs to consume a structure like    f1 (f (a -> b)) with those
-- type constraints and what it really means is ↑ (above) maps to f (g (a -> b))
--
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a)) -- key insight here is that each application of `pure` adds a structure around its argument and in our case, there are two (i.e. 2) applications which makes sense since there are two (2) type constructors in the definition of `Compose`.

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose ((<*>) <$> f <*> a)


-- It pays to understand what's going on here ... so let's dissect the (>>=)
-- implementation. What's going on is essentially this:
--
-- I know i need to lift 'g' into 2-layers of the monad i'm given (i.e. Compose
-- f) which the following type : `Compose f g (Compose f g b)` and now i know
-- that `join :: Monad m => m(m a) -> m a` can actually help me flatten it.
-- Therefore, its being applied. See below.
--
instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure

  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (Compose f) >>= g = join $ fmap g (Compose f)

-- Writer the Foldable instance for Compose 
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose g) = foldMap (foldMap f) g

-- Write the Traversable instance for Compose
-- Let's understand how the expression was constructed.
-- i want to get to the 'a' value embedded in 'Compose f g a' so i lift 'f'
-- once into 'Compose f g a' and that would return 'Compose f g (ff b)' and
-- then i remembered that sequenceA is really good at flipping contexts
-- so i applied it, last.
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative ff => (a -> ff b) -> Compose f g a -> ff (Compose f g b) 
  traverse f (Compose c) = sequenceA $ fmap f (Compose c)


