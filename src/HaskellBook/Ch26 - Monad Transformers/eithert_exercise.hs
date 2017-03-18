{-# LANGUAGE InstanceSigs #-}

module Chapter26_EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- Basically, i need to map 'f' 2-layers (crossing both 'm' and 'Either')
--
instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT (pure (pure a)) 

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT f) <*> (EitherT ma) = EitherT ((<*>) <$> f <*> ma)

instance (Monad m) => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ma) >>= f = 
    EitherT $ do
      v <- ma
      case v of 
          (Left l) -> return (Left l)
          (Right r) -> runEitherT (f r)

-- 
-- The book suggests to build a combinator like swapEither first before
-- expressing swapEitherT interms of swapEither.
--
swapEither :: Either e a -> Either a e
swapEither ea =
  case ea of 
      (Left v) -> Right v
      (Right v) -> Left v

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT ea = EitherT $ (fmap swapEither $ runEitherT ea)


