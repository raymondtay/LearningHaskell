{-# LANGUAGE InstanceSigs #-}

module Chapter26_ReaderT where

import Control.Monad (join)

-- ReaderT is one of the most commonly used transformers in conventional
-- Haskell applications. It is just like Reader, except in the transformer
-- variant we are generating additional structure in the return type of the
-- function.
--
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- At this point in time, the book encourages NOT to read on and instead try
-- out implementing Functor, Applicative and Monad instances for yourselves.
--
-- So, i did.
--


-- Step-by-step, we shall understand.
--
-- (g r) :: m a
-- f :: (a -> b)
-- fmap f (g r) :: m b <-- classic Functor implementation
-- (\r -> fmap f (g r)) :: r -> m b
--
-- slapping a ReaderT infront makes it legit.
--
instance (Functor m) => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT g) = ReaderT $ (\r -> fmap f (g r))

-- Step-by-step, we shall understand.
--
-- ReaderT r m a == r -> m a <-- reminder
-- 
-- Remember the purpose of "pure" is to lift 'a' into the ReaderT context.
-- Next, we don't know what 'm' is and i need to make 'm' an Applicative so
-- that i can make use of 'pure' in the 'm'-context.
--
-- pure a :: m a 
-- (\r -> pure a) :: r -> m a 
-- Slap a ReaderT infront and makes 'pure' legit!
--
--
-- Next, let's look at (<*>) 
--
-- (f r) :: m (a -> b) <-- i already know m ∈ Applicative based on the
-- constraint. This resultant expression is an Applicative!
--
-- (g r) :: m a
-- 
-- (f r) <*> (g r) :: m b <--- classic Applicative 'apply' operation
--
-- (\r -> (f r) <*> (g r)) :: r -> m b
--
-- Slap a ReaderT infront and voila!!! 
--
instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT (\r -> pure a)

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT f) <*> (ReaderT g) = ReaderT (\r -> (f r) <*> (g r))


instance (Monad m) => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  -- f :: r -> m a
  -- (f r) :: m a 
  -- do-syntax with (f r) :: a
  -- (g a) :: ReaderT r m b
  -- runReaderT (g a) :: r -> m b
  -- runReaderT (g a) r :: m b
  -- voila!
  --
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT f) >>= g =
    ReaderT $ (\r ->
      do
        a <- f r
        runReaderT (g a) r)

