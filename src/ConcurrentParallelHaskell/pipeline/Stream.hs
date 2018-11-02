{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveGeneric #-}

-- Here's how we can make use of parallelism between the stages of a pipeline.
-- Each stage of a imaginary pipeline is doing some computation on the stream
-- elements and maintaining state as it does so. When a pipeline stage
-- maintains some state, we cannot exploit parallelism between the stream
-- elements. Instead, we would like each of the pipeline stages to run on a
-- separate core, with the data streaming between them. The `Par` monad will
-- allow us to do exactly that.
-- Before that, we need some ADTs ready; let's take a look at how we can do
-- this.
--
module Stream (
  Stream(..),
  streamFromList,
  mapS,
  foldS,
  parFold
  ) where

import Control.Monad.Par
import Control.DeepSeq
import GHC.Generics (Generic)

-- An IList is a list with an IVar as the tail. This allows the producer to
-- generate the list incrementally, while a consumer runs in parallel, grabbing
-- elements as they are produced. A Stream is an IVar containing an IList.
data IList a = Nil
             | Cons a (IVar (IList a)) deriving (Generic)

instance NFData a => NFData (IList a)

type Stream a = IVar (IList a)

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  v <- new
  fork $ consume xs v
  return v
    where consume [] v = put v Nil
          consume (x:xs) v = do
            tail <- new
            put v (Cons x tail)
            consume xs tail

-- left fold over the Stream and accumulates the result as it traverses over;
-- the thing you need to know is that if the consumer catches up with the
-- producer then the consumer will block while calling the "get inS"
foldS :: (a -> b -> a) -> a -> Stream b -> Par a
foldS fn !acc inS = do
  ilist <- get inS
  case ilist of
      Nil -> return acc
      Cons h t -> foldS fn (fn acc h) t

-- Mapping is both a producer and consumer and it runs as fast as the producer. 
-- Just like `foldS` it will block on the call "get inS" if mapping catches
-- "up", figuratively speaking.
-- Question: can we run > 1 map over the same Stream?
mapS :: NFData b => (a -> b) -> Stream a -> Par (Stream b) 
mapS fn inS = do
  outS <- new
  fork $ consume inS outS
  return outS
    where
      consume inS outS = do
        ilist <- get inS
        case ilist of
            Nil -> put outS Nil
            Cons h t -> do
              newtail <- new
              put outS (Cons (fn h) newtail)
              consume t newtail

parFold :: (NFData a) =>  (a -> a -> a) -> a -> [a] -> a
parFold binop zeroth xs = runPar $ foldS binop zeroth (runPar $ streamFromList xs)

-- | Build: ghc -O2 -threaded -rtsopts -eventlog ./Stream.hs
-- | Run  : ./Stream +RTS -N8 -l -s 
--
main :: IO ()
main = do
  putStrLn $ show (parFold (+) 0 ([1..10000000]::[Int]) )

