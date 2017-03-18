{-# LANGUAGE InstanceSigs #-}

module Chapter26_MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)  }

-- Constructing the functor instance is pretty regular so i won't go in at all
-- but writing the Applicative instance deserves more attention as it isn't
-- straightforward.
--
instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT g) = MaybeT $ (fmap . fmap) f g

-- The "inner"-pure expression directly leverages the Maybe's definition so i
-- know that it returns a `Maybe a` while the "outer"-pure expression wraps the
-- `m`.
--
-- Next, let's proceed to check <*>. What i have basically is the following
-- expression: `m (Maybe (a -> b)) <*> m (Maybe a)` after i reveal the
-- underlying types that correspond to the definition's expression: `(MaybeT f)
-- <*> (MaybeT g)`.
--
-- Basically, i have a function (a -> b) that's embedded in a 2-layer context
-- and i know that:
--
-- ((<*>) <$>)
-- :: (Functor f1, Applicative f) =>
--    f1 (f (a -> b)) -> f1 (f a -> f b)
-- 
-- The KEY idea here is that we have to lift an Applicative "apply" over the
-- outer structure f to get the g(a -> b) into g a -> g b so that the
-- Applicative instance for f can be leveraged. 
-- 
-- The <*> and <$> are left-associative operators of the same precedence and
-- another way to read the implementation for <*> is 
-- (((<*>) <$> f) <*> g)  <--------- read left to right.
-- 
-- and next thing is to work out how the types can be understood proceeding
-- left-to-right fashion.
--
-- The type of 'f' is m (Maybe (a -> b)) and this fits the ((<*>) <$>) 
-- ((<*>) <$>)
-- :: (Functor f1, Applicative f) =>
--    f1 (f (a -> b)) -> f1 (f a -> f b)
--
-- So imagine you apply 'f' to ((<*> <$>) and this returns m (Maybe a -> Maybe b) 
-- and this is basically an Applicative structure since applicatives are always
-- f(a -> b).
--
-- Finally, we apply <*> g to this new Applicative we got. Hence `m (Maybe a ->
-- Maybe b) <*> m (Maybe a)` which returns m (Maybe b) which we wrap MaybeT
-- around.
--
-- Voila !!!!
--
instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT (pure (pure a))

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT f) <*> (MaybeT g) = MaybeT (((<*>) <$> f ) <*>g)
  -- ↑ the same as → (MaybeT f) <*> (MaybeT g) = MaybeT ((<*>) <$> f <*> g)

-- Let me write the Monad instance for my MaybeT 
-- i've interleaved types into each of the expressions so that it helps
-- clarifying how things work....step-by-step.
--
instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = 
    MaybeT $ do
      -- ma :: m (Maybe a)
      -- v :: Maybe a
      v <- ma
      case v of 
          -- return Nothing :: m (Maybe b)
          Nothing -> return Nothing
          -- (f t) :: MaybeT m b
          -- runMaybeT (f t) :: m (Maybe b)
          (Just t) -> runMaybeT (f t)

