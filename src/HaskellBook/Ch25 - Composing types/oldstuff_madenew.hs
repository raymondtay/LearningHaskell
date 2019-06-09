
module OldStuffMadeNew where

import Control.Applicative
import Control.Monad

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) =>  Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


