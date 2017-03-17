{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

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

instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

