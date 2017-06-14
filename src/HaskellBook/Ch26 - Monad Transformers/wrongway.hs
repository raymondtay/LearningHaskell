
{-# LANGUAGE InstanceSigs #-}

module Chapter26_wrongway where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)  }

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ fab <*> mma

