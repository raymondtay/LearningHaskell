{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module SupplyClass(
  MonadSupply(..),
  S.Supply,
  S.runSupply
  ) where

import qualified Supply as S
import System.Random hiding (next)

class (Monad m) => MonadSupply s m | m -> s where
  next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
  next = S.next

-- Thanks to our functional dependency, the type checker now knows that when it
-- sees a type S.Supply s, the type can be used as an instance of the typeclass
-- MonadSupply s.
-- If we didn't have a functional dependency, the type checker would not be
-- able to figure out the relationship between the type parameter of the class
-- MonadSupply s and that of the type Supply s, and it would abort compilation
-- with an error. The definition itself would compile; the type error would not
-- arise until the first time we tried to use it.
--

newtype Reader e a = R { runReader :: e -> a }
instance Functor (Reader e) where
  fmap :: (a -> b) -> Reader e a -> Reader e b
  fmap f g = R $ \e -> f (runReader g e)

instance Applicative (Reader e) where
  pure :: a -> Reader e a
  pure a = R $ \_ -> a
  (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
  f <*> g = R $ \e -> (runReader f e) (runReader g e)

instance Monad (Reader e) where
  return :: a -> Reader e a
  return a = R $ \_ -> a
  (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  m >>= k = R $ \r -> runReader (k (runReader m r)) r

-- How does a piece of code executing in this monad find out what's in its
-- environment? it simply has to "ask":
ask :: Reader e e
ask = R id

-- The clearest motivation for the Reader monad will come later when we discuss
-- combining several monads to build a new monad. There, we will see how to
-- gain finer control over state, so that our code can modify some values via
-- the State monad, while other values remain immutable, courtesy of the Reader
-- monad.
--


newtype MySupply e a = MySupply { runMySupply :: Reader e a } deriving (Monad, Applicative, Functor)

instance MonadSupply e (MySupply e) where
  next = MySupply $ do
    v <- ask
    return (Just v)

-- The above is equivalent to `MySupply (liftM Just ask)` because if you
-- remember what liftM does which is to lift a function 1 layer "below" the
-- targeted monad (which happens to have exactly 1 layer)
--

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)


-- Supplying random numbers
-- if we want to use our Supply monad as a source of random numbers, we have a
-- small difficulty to face. Ideally, we would like to be able to provide it
-- with an infinite stream of random numbers. We can get a StdGen in the IO
-- monad but we must "put back" a different StdGen when we are done. If we
-- don't , the next piece of code to get a StdGen will get the same state as we
-- did. This means that it will generate the same random numbers as we did,
-- which is potentially catastrophic.
--
randomsIO :: Random a => IO [a]
randomsIO =
  getStdRandom $ \g -> 
    let (a, b) = split g
    in (randoms a, b)


