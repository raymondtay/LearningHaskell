
--
-- `{-# LANGUAGE InstanceSigs #-}` 
-- Allows type signatures to be specified in 
-- instance declarations.
--

{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- pure :: a -> Compose f g a
  pure = Compose $ getCompose

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = undefined

