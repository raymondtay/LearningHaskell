{-# LANGUAGE InstanceSigs #-}

-- InstanceSigs is a very helpful GHC extension to helping noobs understand how
-- to navigate the types.

-- Adapted from RWH chapter on State Monads
-- we will start with our type definition, which has exactly the obvious type
-- that we just described; and then we take a look at the real thing.
newtype SimpleState s a = SimpleState { run :: s -> (a, s) }

-- our monad is a function that transforms one state into another, yielding a
-- reuslt when it does so. because of this, the State monad is sometimes called
-- the state transformer monad.
--

instance Functor (SimpleState s) where
  fmap :: (a -> b) -> SimpleState s a -> SimpleState s b
  fmap f (SimpleState g) = SimpleState $ (\s -> let (a, s2) = g s in (f a, s2))

instance Applicative (SimpleState s) where
  pure :: a -> SimpleState s a
  pure a = SimpleState (\s -> (a, s))
  (<*>) :: SimpleState s (a -> b) -> SimpleState s a -> SimpleState s b
  f <*> g = SimpleState (\s -> let (k, s2) = run f $ s
                                   (k2, s3) = run g $ s2
                               in (k k2, s3)
                        )

instance Monad (SimpleState s) where
  return :: a -> SimpleState s a
  return = pure
  (>>=) :: SimpleState s a -> (a -> SimpleState s b) -> SimpleState s b
  f >>= g = SimpleState $ \s -> let (a, s2) = run f $ s
                                    (a2, s3) = (run $ g a) $ s2
                                in (a2, s3)

-- The State Monad, by contrast, accepts any type of state as a parameter. We
-- supply the type of the state to give, for example, State ByteString. The
-- State Monad wil lprobably feel more familiar to you than many other monads
-- if you have a background in imperative languages. After all, imperative
-- languages are all about carrying around some implicit state, reading some
-- parts, and modifying others through assignment, which is just what the STate
-- monad is for.
--
-- Running the State Monad. Each Monad has its own specialized evaluation
-- functions. In the case of the State Monad, we have several to choose from:
-- runState - returns both the result and final state
-- evalState - returns only the result, discarding the final state
-- execState - returns only the final state, discarding the result
-- Note: this is coincidentally the convention chosen by Scalaz and later on by
-- Cats.


