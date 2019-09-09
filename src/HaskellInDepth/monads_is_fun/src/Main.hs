{-# LANGUAGE FlexibleContexts #-}

module Main where

import MonadsAreFun
import Control.Monad.Reader
import Control.Monad.RWS
import Numerals

type Spewer =
  RWS 
    Config
    [String]
    ()

spewDetails :: Spewer ()
spewDetails = do
  cfg <- ask
  tell ["A configuration object landed."]
  tell [spewFlag cfg, spewKey cfg, spewValue cfg]
    where
      spewFlag cfg  = "Param: [Flag]: " ++ (show . flag $ cfg) ++ ","
      spewKey cfg   = "Param: [Key]: " ++ (show . key $ cfg) ++ ","
      spewValue cfg = "Param: [Value]: " ++ (show . value $ cfg)

work :: ConfigM ()
work = do
  -- liftIO $ fictiousWork c
  -- liftIO . putStrLn $ "[work] is given this configuration: " ++ (show c)
  pure ()

fictiousWork :: Config -> IO ()
fictiousWork c = do
  _ <- consume c
  liftIO . putStrLn $ "[work] is given this configuration: " ++ (show c)
    where
      consume cfg = do
        putStrLn $ "[work] key " ++ (show . key $ cfg)
        putStrLn $ "[work] flag " ++ (show $ asks flag cfg)
        putStrLn $ "[work] value " ++ (show . value $ cfg)

g :: ConfigM ()
g = do
  c <- ask
  local (\x -> x { flag = False }) f

f :: Reader Config ()
f = do
  f1 <- asks flag
  -- void $  putStrLn . show $ f1
  pure ()

-- pretty generic
getKey :: MonadReader String m => m String
getKey = do
  k <- ask
  return k

-- pretty generic
getFlag :: MonadReader b m => m b
getFlag = do
  k <- ask
  return k

-- pretty generic
getValue :: MonadReader b m => m b
getValue = do
  k <- ask
  return k

main :: IO ()
main = do
  putStrLn $ show $ convertToExpr "(4*2) + (2+8*7)" -- demonstration of `Numerals` module
  putStrLn "Hello, Haskell!"
  putStrLn $ "One way to do it: " ++ (show ((Config <$> getValue . getFlag . getKey) "name" True "value"))
  pure $ runReader work (Config "name" False "45")
  putStrLn . join . snd $ evalRWS spewDetails (Config "KAK" True "KAKAKAKA") ()
  fictiousWork (Config "name" False "45")
  pure $ runReader g (Config "name" True "45")
