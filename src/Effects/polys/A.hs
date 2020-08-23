{-# LANGUAGE TypeApplications #-}

module A where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import qualified Polysemy.Internal as P
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State

data DataStore m a where
  ReadFromStore :: DataStore m Int
  WriteToStore :: Int -> DataStore m Int

makeSem ''DataStore


store' :: Member DataStore r => Int -> Sem r Int
store' x = P.send (WriteToStore x :: DataStore (Sem r) Int)

read' :: Member DataStore r => Sem r Int
read' = P.send (ReadFromStore)

addToStore :: Member DataStore r => Sem r Int
addToStore = read' >>= store'

runI :: Member (Embed IO) r => Sem (DataStore ': r) a -> Sem r a
runI = interpret \case
  ReadFromStore  -> embed $ print "Please enter: " >> getLine >>= \x -> return $ read @Int x
  WriteToStore x -> embed $ (putStrLn $ "You gave me: " ++ (show x)) >> return x

execIt = runM . runI $ addToStore

