{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AboutMonadTrans where

import Control.Monad.State

{-
 MonadTrans is a typeclass with one core method: lift. Speaking generally, it is about lifting actions
 in some Monad over a transformer type which wraps itself in the original Monad. Fancy ! 
-}

class MonadTrans t where
  -- | lift a computation from the argument monad to the 
  --   constructed monad ('t' is the constructed monad.)
  lift :: (Monad m) => m a -> t m a 

newtype ScottyT e m a = ScottyT { runS :: State (ScottyState e m) a } deriving (Functor, Applicative, Monad)

newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e ) (ReaderT ActionEnv (StateT ScottyResponse m)) a } deriving (Applicative, Functor)

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO

