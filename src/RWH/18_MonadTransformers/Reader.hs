{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving, DeriveFunctor #-}

--
-- Demo of the MonadReader
--

import Control.Monad.Reader
import Control.Monad.State

tick :: State Int Int
tick = do
  i <- get
  return $ i+1

addOne :: Reader Int Int
addOne = do i <- ask
            return (i+1)

addTwo :: Reader Int Int
addTwo = do i <- ask
            local (+1) addOne -- modify the environment, addOne is run against the result of the "local" op.

mulTwoPlusOne :: Reader Int Int
mulTwoPlusOne = do i <- ask
                   local (*2) addOne -- modify the environment, addOne is run against the result of the "local" op.

-- Run the following like this:
-- > runReader addOne 4
-- > 5
-- > runReader addTwo 4
-- > 6
-- > runReader mulTwoPlusOne 2
-- > 5

{-
 - This resource on MTL (https://wiki.haskell.org/Monad_Transformers_Tutorial) is actually 
 - important in the respect that beginners often wonder how the design of the monad transformers
 - actually came about; to the absolute beginner it is important to understand how it started
 - , what drove it to the state it is today etc. Recommended reading.
 -}

newtype MaybeIO a = MaybeIO { 
  runMaybeIO :: IO (Maybe a)
} deriving (Functor)

instance Applicative MaybeIO where
  pure x = MaybeIO (return (Just x))
  (<*>) :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b -- helpful, to me, to visualize the signature
  MaybeIO f <*> MaybeIO action = MaybeIO $ do
    g <- f
    r <- action
    return $ g <*> r

instance Monad MaybeIO where
  return x = MaybeIO (return (Just x))
  MaybeIO action >>= f = MaybeIO $ do
    result <- action
    case result of
        Just v -> runMaybeIO (f v)
        Nothing -> return Nothing

transformIOtoMaybeIO :: IO a -> MaybeIO a
transformIOtoMaybeIO action = MaybeIO $ do
  result <- action
  return (Just result)


