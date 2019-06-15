
{-# LANGUAGE InstanceSigs #-}

-- refresher on 15 June 2019...need to keep brains a.c.t.i.v.e
--
module Chapter26_wrongway where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)  }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT $ (pure . pure) a

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT f) <*> (MaybeT a) = MaybeT $ (<*>) <$> f <*> a

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return a = pure a

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) f = MaybeT $ do
    m <- ma
    case m of 
        Nothing -> return Nothing
        Just a -> runMaybeT (f a)


-- instance Applicative m => Applicative (MaybeT m) where
--   pure x = MaybeT (pure (pure x))
--   (MaybeT fab) <*> (MaybeT mma) = MaybeT $ fab <*> mma

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f $ ema

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ (pure . pure) a

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emf) <*> (EitherT ema) = EitherT $ (<*>) <$> emf <*> ema

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return a = pure a

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ema) >>= f = EitherT $ do
    em <- ema
    case em of
        Left l -> return (Left l)
        Right r -> runEitherT (f r)


swapEither :: Either a b -> Either b a
swapEither (Right b) = Left b
swapEither (Left a) = Right a


swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither $ ema


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT $ \r -> pure a

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ \r -> (rmf r) <*> (rma r)

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: (ReaderT r m a) -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

newtype StateT s m a = StateT { runStateT :: s -> m (a , s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT g) =
    StateT $ (\s -> fmap (\pair -> (,) ((f . fst) pair) (snd pair)) (g s))

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) =
    StateT $ \s -> do
      mf <- smf s
      ma <- sma s
      return ((fst mf) (fst ma), s)

