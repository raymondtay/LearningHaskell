{-# LANGUAGE FunctionalDependencies #-}

class (Monad m) => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r ) -> m a -> m r


