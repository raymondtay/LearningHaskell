
module GettingStartedWithWriterMonads where

import Control.Monad.Writer 

-- Easier to understand monad-transformers from building simple functions
-- and factor the code to build more knowledge.
factorial :: Int -> Int
factorial n = go n 1
  where go 1 acc = acc
        go x acc = go (x -1) (x*acc)

factorial2 :: Int -> Writer [String] Int
factorial2 n = go n 1
  where go :: Int -> Int -> Writer [String] Int
        go 1 acc = writer (acc, ["Done: " ++ show acc])
        go x acc = do
          tell [ "So far i got: " ++ show (x-1)]
          go (x -1) (x*acc)

factorial3 :: Int -> Writer [String] Int
factorial3 n = go n 1
  where go :: Int -> Int -> Writer [String] Int
        go 1 acc = do
          listen $ do { return 45 }
          writer (acc, ["Done: " ++ show acc])
        go x acc = do
          tell [ "So far i got: " ++ show (x-1)]
          go (x -1) (x*acc)

factorial4 :: Int -> Writer [String] Int
factorial4 n = go n 1
  where go :: Int -> Int -> Writer [String] Int
        go 1 acc = do
          listen $ do { return 45 }
          pass $ do
            a <- writer (acc, ["Done: " ++ show acc])
            return (a, \d -> ((++) "!!!") <$> d)
        go x acc = do
          tell [ "So far i got: " ++ show (x-1)]
          go (x -1) (x*acc)



