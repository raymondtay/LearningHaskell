{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply (
  Supply, next, runSupply 
              ) where

import Control.Monad.State

-- The 's' parameter is the type of the unique values we are going to supply
-- and 'a' is the usual type parameter that we must provide in order to make
-- our type a monad.
-- 
-- In the haskell book that i bought, the importance of working through
-- developing an intuition of developing instances of Functors, Applicatives
-- and Monads. Turns out we can remove all these boilerplate by using the
-- language pragma 'GeneralizedNewtypeDeriving' and i can clearly see the
-- derivation is done successfully since it loaded into GHCi with no visible
-- problems. Here's what it looks like:
-- *Supply Seq Data.Sequence Control.Monad Control.Arrow Control.Monad.State> :i Supply
-- type role Supply nominal nominal
-- newtype Supply s a = S (State [s] a)
-- Defined at Supply.hs:13:1
-- instance Monad (Supply s) -- Defined at Supply.hs:13:71
-- instance Functor (Supply s) -- Defined at Supply.hs:13:49
-- instance Applicative (Supply s) -- Defined at Supply.hs:13:58
-- *Supply Seq Data.Sequence Control.Monad Control.Arrow Control.Monad.State>
--
newtype Supply s a = S( State [s] a ) deriving (Functor, Applicative, Monad)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of 
                  [] -> return Nothing
                  (x:xs) -> do put xs
                               return (Just x)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

--instance Monad (Supply s) where
  --s >>= m = S (unwrapS s >>= unwrapS . m)

