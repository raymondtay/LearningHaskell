
import Control.Monad.Trans

{- 
  To give ourselves some insight into how monad transformers in general work, we will
  create one and describe its machinery as we go. Our target is simple and useful:
  `MaybeT`. This is still missing from the MTL.

  This monad transformer modifies the behavior of an underlying monad `m a` by wrapping
  its type parameter with `Maybe`, in order to get `m (Maybe a)`. As with the `Maybe` monad,
  if we call 'fail' in the 'MaybeT' monad transformer, execution terminates early.
  
  In order to turn 'm (Maybe a)' into a 'Monad' instance, we must make it a distinct type
  via a 'newtype' declaration
-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- The trick to understanding the body of our (>>=) implementation is that everything inside the
-- 'do'-block executes in the underlying monad m, whatever it is:
--
bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT m f = MaybeT $ do
  unwrapped <- runMaybeT m
  case unwrapped of 
    Nothing -> return Nothing
    Just x -> runMaybeT (f x)

