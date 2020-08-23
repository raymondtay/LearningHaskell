{-# LANGUAGE TypeApplications #-}

module B where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import qualified Polysemy.Internal as P
import           Polysemy.Output
import           Polysemy.Reader
import           Polysemy.State

import qualified System.Random     as R

data Console m a where
  PrintLine :: String -> Console m ()
  ReadLine :: Console m String

data Converter m a where
  ConvertToInt :: String -> Converter m Int

data Random v m a where
  NextRandom :: Random v m v

makeSem '' Console
makeSem '' Converter
makeSem '' Random

program :: (Member Console r, Member (Random Int) r) => Sem r Int
program = do
  printLine "Insert your number"
  x <- readLine
  y <- nextRandom
  pure (read x + y)

runConsoleIO :: Member (Embed IO) r => Sem (Console ': r) a -> Sem r a
runConsoleIO = interpret \case
  PrintLine line -> embed $ putStrLn line
  ReadLine       -> embed $ getLine

runRandomIO :: Member (Embed IO) r => Sem (Random Int ': r) a -> Sem r a
runRandomIO = interpret \case
  NextRandom -> embed R.randomIO

execute :: IO ()
execute = do
  result <- runM . runConsoleIO . runRandomIO $ program
  putStrLn $ "The answer: " ++ (show result)


