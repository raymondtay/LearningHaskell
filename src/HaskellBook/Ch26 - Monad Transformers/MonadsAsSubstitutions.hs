module MonadsAsSubstitutions where

import Control.Monad

data Type v = TVar v -- type variable
            | TInt -- integer type
            | Fun (Type v) (Type v) -- function type
{--
instance Text v => Text (Type v) where
  showsPrec p (TVar v) = shows v
  showsPrec p TInt = showString "Int"
  showsPrec p (Fun l r) = showParen (p>0) str
    where str = showsPrec 1 l . showString " -> " . shows r
--}

instance Functor Type where 
  fmap f (TVar v) = TVar (f v)
  fmap f TInt = TInt
  fmap f (Fun d r) = Fun (fmap f d) (fmap f r)

instance Applicative Type where
  pure = TVar
  (TVar f) <*> (TVar a) = TVar (f a)
  (Fun df rf) <*> (Fun d r) = Fun (df <*> d) (rf <*> r)

instance Monad Type where
  return v = TVar v
  (TVar v) >>= f = f v
  TInt >>= f = TInt
  (Fun d r) >>= f = Fun (d >>= f) (r >>= f)

apply :: Monad m => (a -> m b) -> (m a -> m b) 
apply f t = t >>= f

-- Composition of substitutions also corresponds to a more general operator,
-- called Kleisli composition, that can be used with arbitrary monads. Written
-- here as the infix operator(@@), Kleisli composition can be defined as 
--
(@@) :: Monad m => (a -> m b) -> (c -> m a) -> (c -> m b)
f @@ g = join . fmap f . g


