module Chap21 where

import GHC.Base hiding ((,))
import GHC.Tuple hiding ((,))
import Prelude hiding (Left, Right, Either)

data Either a b = 
  Left a 
  | Right b
  deriving (Eq, Show, Ord)

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either a) where
  pure = Right -- why not Left u might ask? Left normally implies something went wrong and Right represents otherwise
  Left f <*> _ = Left f
  Right f <*> r = fmap f r


instance Foldable (Either a) where
  foldMap _ (Left _) = mempty -- by convention, we don't evaluate the Left condition as it represents an awry situation
  foldMap f (Right y) = f y

  foldr _ z (Left _ ) = z
  foldr f z (Right y) = f y z

instance Traversable (Either e) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y

-- Tuple
--

instance Functor((,) a) where
  fmap f (x, y) = (x, f y)

instance Monoid a => Applicative ((,) , a) where 
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

instance Foldable ((,), a) where
  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap                                 f           (_, y) = f y
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr f z (_, y) = f y z


