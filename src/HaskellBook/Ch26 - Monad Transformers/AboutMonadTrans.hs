{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module AboutMonadTrans where

import Control.Monad.Except
import Control.Monad.Trans.State

{-
  MonadTrans is a typeclass with one core method: `lift`. Speaking generally,
  it is about lifting actions in some Monad over a transformer type which wraps
  itself in the original Monad. Fancy ! 
-}

class MonadTrans t where
  -- | lift a computation from the argument monad to the 
  --   constructed monad ('t' is the constructed monad.)
  lift :: (Monad m) => m a -> t m a 


data TickTok a = TickTok { numbers :: [a] } deriving (Eq, Show, Functor)

ticktok :: State (TickTok Int) Int
ticktok = do
  tt <- get
  put . TickTok $ (numbers tt) ++ [(((last . numbers) $ tt) + 1)]
  tt' <- get
  return (last . numbers $ tt')

data ErrorMessage e = CompleteFailure e | NotATotalFailure deriving (Show, Eq)

newtype TickTokT a = TickTokT { runS :: State (TickTok a) a }

newtype ActionT e a =
  ActionT { runIt :: ExceptT (ErrorMessage e) (State (TickTok Int) ) a}


