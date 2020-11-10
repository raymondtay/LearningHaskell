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
  LoadFromEnv :: DataStore m Int
  ReadFromCLI :: DataStore m Int
  WriteToStore :: Int -> DataStore m Int

makeSem ''DataStore


store' :: Member DataStore r => Int -> Sem r Int
store' x = P.send (WriteToStore x :: DataStore (Sem r) Int)

read' :: Member DataStore r => Sem r Int
read' = P.send (ReadFromCLI)

addToStore :: Member DataStore r => Sem r Int
addToStore = read' >>= store'

runI :: (Member (Reader Int) r, Member (Embed IO) r) => Sem (DataStore ': r) a -> Sem r a
runI = interpret \case
  LoadFromEnv    -> do
    r <- ask @Int
    return (r+2)
  ReadFromCLI    -> embed $ print "Please enter: " >> getLine >>= \x -> return $ read @Int x
  WriteToStore x -> embed $ (putStrLn $ "You gave me: " ++ (show x)) >> return x

execIt = runM . runReader 9 . runI $ addToStore

