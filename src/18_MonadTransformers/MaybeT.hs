{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             InstanceSigs #-}

module MaybeT where

-- Understanding Monad Transformers by Building one
-- Instructive to learn from RWH, Haskell Programming books etc
-- Better to keep all source code in single source file, for now.
--

import Control.Monad        (liftM)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT g) = MaybeT $ (fmap . fmap) f g

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT (pure (pure a))
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (<*>) (MaybeT f) (MaybeT g) = MaybeT $ (((<*>) <$> f) <*> g)

instance Monad m => Monad (MaybeT m) where
  return  :: a -> MaybeT m a
  return a = MaybeT (return (return a))
  -- return = pure -- this works too and less verbose then the previous.

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT f) >>= g = MaybeT $ do h <- f
                                 case h of
                                     Nothing -> return Nothing
                                     Just v -> runMaybeT $ g v

-- Creating a Monad Transformer
-- To turn out new swanky type up there ↑ , we must provide an instance of the
-- MonadTrans class so that a user can access the underlying monad
--

-- Tip: The trick to understanding the body of the implementation is that
-- everything inside the "do" block executes in the underlying monad m,
-- whatever that is:
--
instance MonadTrans MaybeT where
  lift m = MaybeT (liftM Just m)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get :: MaybeT m s
  get = lift get
  put :: s -> MaybeT m ()
  put k = lift (put k) 

-- Remember: when you are writing instances for the typeclass you have to put
-- yourself into the perspective of the underlying monad, whatever it is.
-- It can be frustrating from time to time to have to remind yourself of this.
--
instance (MonadReader r m) => MonadReader r (MaybeT m) where
  ask :: MaybeT m r
  ask = ask
  local :: (r -> r) -> MaybeT m a -> MaybeT m a
  local = local
  reader :: (r -> a) -> MaybeT m a
  reader = reader


