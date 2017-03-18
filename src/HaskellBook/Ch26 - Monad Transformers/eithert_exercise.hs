{-# LANGUAGE InstanceSigs #-}

module Chapter26_EitherT where

import Data.Either

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
-- It's pretty easy to develop this small function to swap Either values
-- so i can move on to the next function, swapEitherT.
swapEither :: Either e a -> Either a e
swapEither ea =
  case ea of 
      (Left v) -> Right v
      (Right v) -> Left v

-- 
-- This function requires some thought ... and i made use of the fact that we
-- have a functor to lift swapEither into the embedded structure EitherT e m a
-- which works out to be `m (Either a e)` so i use runEitherT to extract the
-- embedded value and since `m` is functor then i can use `fmap swapEither` to 
-- access the Either values in the `m (Either a e)` structure.
-- Voila !!!!
--
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT ea = EitherT $ (fmap swapEither $ runEitherT ea)

-- This is trickier than the last because it has more symbols !!!
-- Let's dissect this further, shall we?
-- My way of doing this is rather simple, considering my skills at this point
-- isn't God-like....
--
-- EitherT a m b is equivalent to `m (Either a b)` so i basically use the
-- combination of the do-syntax to extract either embedded Either value out and
-- pattern match to discover whether i'm seeing a Left or Right and applying
-- either f or g accordingly.
--
-- In Scala's CATS, there's a notion of `bimap` which basically does the same
-- thing.
--
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g et = 
  do
    v <- runEitherT et 
    case v of 
        (Left l) -> f l
        (Right r) -> g r

-- This was written like 10 minutes later coz i remember CAT's bimap has a
-- similar functionality, i was eager to find out for myself whether something
-- like it exists. Turns out there is, its in `Data.Either` and the function is
-- `either`.
-- Hence, proceeding as usual i use do-syntax combined with runEitherT to
-- extract the Either a e value and leverage `either`.
--
eitherT' :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT' f g et =
  do 
    v <- runEitherT et
    either f g v


