{-# LANGUAGE InstanceSigs #-}

module Chapter26 where

import Control.Monad (liftM)

newtype MaybeT m a = MaybeT' { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT' ma) = MaybeT' $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT' . pure . pure

  (MaybeT' fab) <*> (MaybeT' mma) = MaybeT' $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT' ma) >>= f =
    MaybeT' $ do
      v <- ma -- we know `ma` is a Monadic value, so `v` gets the Maybe value lifted out
      case v of  -- since we know `v` is a Maybe value, we can pattern match it using case expressions
        Nothing -> return Nothing
        Just y  -> runMaybeT (f y) -- at this point, we know y is a value of type `a` and we apply `f` to it which gives `MaybeT m b`


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema
--
-- example usage:
--
-- let x = EitherT $ Just (Right 4) in fmap (+1) x
--
instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  f <*> a = EitherT $ (<*>) <$> (runEitherT f) <*> (runEitherT a)
  -- (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma -- equivalent to the above !!!


instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  y >>= f =
    EitherT $ do
      y' <- (runEitherT y)
      case y' of 
        Left y'' -> return (Left y'')
        Right y''' -> runEitherT (f y''')


-- 
-- From https://hackage.haskell.org/package/either-4.4.1.1/src/src/Control/Monad/Trans/Either.hs
--
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT 

-- 
-- From https://hackage.haskell.org/package/either-4.4.1.1/docs/src/Data.Either.Combinators.html#swapEither
--
swapEither :: Either r l -> Either l r
swapEither = either Right Left

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT m) = m >>= (\x -> case x of
  Left a -> f a
  Right b -> g b)


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- remember that 'g' is 'r -> m a' 
-- and so we want to extract 'm a' from 'g' by doing 'g r'
-- and finally since 'f' is 'a -> b' so we fmap again
-- over 'm a' which means 'm' needs to be a functor
instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT g) = ReaderT $ \r -> (<$>) f (g r) 


instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure = ReaderT . pure . pure 

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = 
    ReaderT $ \r -> do
      x <- rma r -- remember that 'rma' is actually r -> m a
      runReaderT (f x) r  -- remember that runReaderT (f x) gives us 'ReaderT r m b' which is 'r -> m b' but we need a 'm b' so we consume 'r'


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- fmap f (StateT sma) = StateT $ \s ->
  --   let mas = sma s  -- gives us 'm (a, s)' where 'm' ∈ Functor
  --   in fmap (\t -> (f (fst t), s)) mas
  fmap f (StateT sma) = StateT $ \s -> fmap (\tuple -> (f (fst tuple), s)) (sma s)


instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT (\s -> return (a, s)) -- because the constraint for 'm' is a Monad, so i use 'return' to wrap '(a, s)' as a Monad of whatever type

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
      (f, s1) <- smf s
      (a, s2) <- sma s
      return (f a, s)

  -- This took me a long time (approx 2 hrs to solve). The key to this mystery is
  -- recognizing that `smf s` returns a `m (a -> b, s)`  and `sma s` returns a `m (a, s)` which 
  -- are both Monads !!!!
  -- That means we can use the `do` syntax to extract the values out which would end
  -- up as a `(a -> b, s)` and `(a, s)` respectively 
  -- The last thing we do is wrap the function application back into a Monad !!! remember that!
  -- which means we need to say `return (f a, s)`


instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b)  -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s1) <- sma s
    (b, s2) <- runStateT (f a) s
    return (b, s)

