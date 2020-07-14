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
import Control.Monad.Writer
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

  fail :: String -> MaybeT m a -- a good idea to provide an interpretation of what to do here.
  fail _ = MaybeT $ return Nothing

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
-- instance (MonadReader r m) => MonadReader r (MaybeT m) where
--   ask :: MaybeT m r
--   ask = ask
--   local :: (r -> r) -> MaybeT m a -> MaybeT m a
--   local = local
--   reader :: (r -> a) -> MaybeT m a
--   reader = reader

-- Adding this instance of MonadWriter over here so that the compilation for
-- [[StackingOrderMatters.hs]] for "b" will pass.
-- Consult https://github.com/bos/rwh/blob/master/examples/ch18/MaybeT.hs if
-- 
--
instance (Monoid w, MonadWriter w m) => MonadWriter w (MaybeT m) where
  -- shouts to the monad
  tell = lift . tell -- "lift" here will lift the inner m to this level
  -- listens to the monad acting, and returns what the monad "said" according
  -- to the source for MonadWriter. Seems like the idea is for me to evaluate
  -- all the way "in" and return what is going on
  listen :: MaybeT m a -> MaybeT m (a , w)
  listen m = MaybeT $ do
    (result, logs) <- listen (runMaybeT m) -- run the inner monad and then listen to it
    case result of
        Nothing -> return Nothing
        Just v -> return (Just (v, logs))

  -- pass is an action that executes the monad m which returns a value and a
  -- function, and returns the value, applying the function to the output.
  pass :: MaybeT m (a, w -> w) -> MaybeT m a
  pass m = MaybeT $ do
    a <- runMaybeT m -- returns a value of this type: Maybe (a, w -> w)
    case a of
        Nothing -> return Nothing
        Just (v, log) -> pass $ return (Just v, log)

