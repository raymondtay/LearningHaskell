{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- The above stanza (↑) is very handy in removing boilerplate found below (↓)

module Supply ( Supply, next, runSupply) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) -- i.e. S ( (\[s] -> (a, [s])) ) where 's' is the type of the unique values we supply

-- To widen our scope, we will move beyond random numbers and implement a
-- monad that supplies unique values of any kind. The name we will give to our
-- monad is Supply. We will provide the execution function, runSupply, with a
-- list of values (it will be up to us to ensure that each one is unique).
--

-- runSupply :: Supply s a -> [s] -> (a, [s])
-- runSupply = undefined

-- The monad won't care what the values are. They might be random numbers or
-- names for temporary files, or identifiers for HTTP cookies.
-- Within the monad, every time a consumer asks for a value, the next action
-- will take the next one from the list and give it to the consumer. Each value
-- is wrapped in a Maybe constructor in case the list isn't long enough to
-- satisfy the demand:
--
-- next :: Supply s (Maybe s)
-- next = undefined

-- i find its almost always a good practice to implement some boilerplate when
-- you are starting out with haskell, for the fact that practice makes perfect.
instance Functor (Supply s) where
  fmap :: (a -> b) -> Supply s a -> Supply s b
  -- f `fmap` (S g) = S( state (\s -> let (a, ss) = runState g $ s in (f a, ss)) )
  fmap f (S g) = S (f <$> g) -- this is another way to compress the definitions after realizing the fact that `State` is indeed a Functor.

instance Applicative (Supply s) where
  pure :: a -> Supply s a
  pure a = S ( state (\s -> (a, s)) )
  (<*>) :: Supply s (a -> b) -> Supply s a -> Supply s b
  (S f) <*> (S g) = S ( state (\s -> let (h, s1) = runState f $ s
                                         (a, s2) = runState g $ s1
                                     in (h a, s2)))
instance Monad (Supply s) where
  return :: a -> Supply s a
  -- return a = S (state (\s -> (a, s))) -- this is typically how i would have written it but apparently there's another way
  return = S . return -- this is it! and the interesting fact that why this would work is, according to me that is:
  -- i knew that `return` injects a value of type `a` and returns a Monad, i
  -- guess you knew that too since `return :: Monad m => a -> m a` and when we
  -- apply that monad i.e. `m a` to the input of `S :: State [s] a -> Supply s a` you should read it as 
  -- `S :: Some Monad -> Supply s a` and that's why the expressions `S .
  -- return` works !
  (>>=) :: Supply s a -> (a -> Supply s b) -> Supply s b
  (S f) >>= g = S (f >>= unwrapS . g)

-- unwrapping the state function carried in 'S'
unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

-- After the previous effort in providing instances for the typeclasses i was
-- interested in, and what we have gain here is very useful beyond just this
-- example. We can use `newtype` to wrap any underlying type; we selectively
-- expose only those typeclass instances that we want; and we expend almost no
-- effort to create these narrower, more specialized types.
--
--
next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                  [] -> return Nothing
                  (x:xs) -> do put xs
                               return (Just x)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs


-- Another important way to make code more modular involves separating its
-- interface (what the code can do) from its implementation - how it does it.
-- The standard random number generator in `System.Random` is known to be quite
-- slow. If we use our `randomsIO` function to provide it with random numbers,
-- then our next action will not perform well.


