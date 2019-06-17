{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module ReaderWorkout where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

--myName :: String -> String -> String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)


localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "first"
  b <- local (++"dy") (myName "second")
  c <- myName "Third"
  return (a, b, c)

localExample2 :: Reader String (String, String)
localExample2 = do
  a <- ask
  b <- local ((++"@<@") :: String -> String) ask -- this "ask" will get the same value as the previous "ask"
  return (a, b)


data AppConfig = AppConfig { cfgMaxDepth :: Int  } deriving Show
data AppState = AppState { stDeepestReached :: Int } deriving Show

type App = ReaderT AppConfig (StateT AppState IO)

newtype MyApp a =
  MyA {
    runA :: ReaderT AppConfig (StateT AppState IO) a
  } deriving (Monad, MonadIO, MonadReader AppConfig, Functor, Applicative, MonadState AppState)


runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
   in runStateT (runReaderT (runA k) config) state


