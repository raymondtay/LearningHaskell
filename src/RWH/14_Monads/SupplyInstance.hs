{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances,
   MultiParamTypeClasses, FunctionalDependencies #-}

import SupplyClass
import System.Random hiding (next)

newtype Reader e a = R { runReader :: e -> a } deriving (Functor, Applicative)


instance Monad (Reader e) where 
  return a = R $ \_ -> a
  m >>= k = R $ \r -> runReader (k (runReader m r)) r

-- Our definition of our value of tyep 'e' as an environment in which we are
-- evaluating some expression. Then 'return' action should have the same effect
-- no matter what the environment is, so our version ignores its environment.
--
-- Our definition of (>>=) is a little more complicated, but only because we
-- have to make the environment - here the variable 'r' - available both in the
-- current computation and in the computation we are chaining into.
--
-- How does a piece of code executing in this monad find out whats in its
-- environment ? It simply has to ask:
--

ask :: Reader e e
ask = R id

-- An example of how it would work is as follows:
--
-- *Main> runReader (ask >>= \x -> return (x * 2)) 2
-- 4
-- *Main> runReader (ask >>= \x -> return (x * 3)) 2
-- 6
-- 

newtype MySupply e a = MySupply { runMySupply :: Reader e a  } deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
  next = MySupply $ do 
          v <- ask
          return (Just v)
  -- more concise:
  -- next = MySupply (Just `liftM` ask)

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)


randomsIO :: Random a => IO [a]
randomsIO =
  getStdRandom $ \g ->
    let (a, b) = split g
    in (randoms a, b)


runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
