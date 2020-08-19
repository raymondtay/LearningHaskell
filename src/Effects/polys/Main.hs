module Main where

-- What i want to do is to use polysemy to implement an demonstration of
-- outputing the results of a random number to the console.
--

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

ttyToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
ttyToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTtyPure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTtyPure i = runOutputMonoid pure
  . runInputList i
  . reinterpret2 \case
      ReadTTY    -> maybe "" id <$> input
      WriteTTY msg -> output msg


echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo


echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTtyPure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

main :: IO ()
main = runM . ttyToIO $ echo

