{-# LANGUAGE OverloadedStrings #-}

import Network.AMQP.Worker
import Control.Concurrent.STM
import Control.Exception
import System.Environment (getArgs)

type Order = (String, Integer)

ordersQueue :: Queue Direct Order
ordersQueue = let testExchange = exchange "test" in queue testExchange "orders"

initialize :: IO Connection
initialize = do
  conn <- connect (fromURI "amqp://guest:guest@localhost:5672")
  initQueue conn ordersQueue
  return conn

frontend :: Connection -> IO ()
frontend conn = do (product, price) <- return ("a", 800)
                   publish conn ordersQueue (product, price)
                   putStrLn "Message sent"

backend :: Connection -> IO ()
backend conn = do
  m <- newTVarIO 1000
  s <- newTVarIO [("a", 7)]
  putStrLn "Starting backend ..."
  worker def conn ordersQueue onBackendError (onBackendMessage m s)

onBackendMessage :: TVar Integer -> TVar [(String, Integer)] -> Message Order -> IO ()
onBackendMessage m s Message { value = (product, price) } = do putStrLn $ "Received order " ++ show (product, price)
                                                               atomically $ pay product price m s

onBackendError :: WorkerException SomeException -> IO ()
onBackendError e = putStrLn $ "ERROR: " ++ show e

main :: IO ()
main = do conn <- initialize
          args <- getArgs
          case args of 
            "backend" : _ -> backend conn
            "frontend" : _ -> do frontend conn
                                 _ <- getLine -- wait for completion
                                 return ()


