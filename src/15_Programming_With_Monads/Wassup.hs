{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving #-}

module Wassup (
  Supply, next, runSupply
  ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) deriving (Functor, Applicative, Monad) -- boilerplate reduction ! How cool is that !

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

{-
     
instance Monad (Supply s) where
  return = S . return
  s >>= m = S (unwrapS s >>= unwrapS . m)

-}

-- what this code is doing is that it obtains the current state and interprets
-- whether its a Just or a Nothing and if in the former situation, it would be
-- store the "tail" into the state whilst returning the "head".
next :: Supply a (Maybe a)
next = S $ do st <- get
              case st of
                  [] -> return Nothing
                  (x:xs) -> do put xs
                               return (Just x)

-- when `runState m` is applied, it returns `s -> (a, s)` which is actually `[s] -> (a, [s])`
-- and finally it consumes "xs" which is `[s]` and eventually returns `(a, [s])`
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

{-
    
  The pattern of applying a function to one element of a pair and constructing a new pair with the other original element 
  untouched is common enough in Haskell code that it has been turned into standard code.

-}

