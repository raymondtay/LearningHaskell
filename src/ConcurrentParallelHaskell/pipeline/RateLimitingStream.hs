{-# LANGUAGE BangPatterns, FlexibleInstances, FlexibleContexts, DeriveGeneric #-}

-- From Simon Marlow's book Page 68, he goes to explain how we can rate-limit
-- the producer because the consumer was moving faster than the producer which
-- causes the consumer to wait for the producer. Talk about repeating nouns in
-- a single sentence, my english teacher would have given me hell for it.
--
-- Apparently, there's a trick and let's see how it can be put together. The
-- idea is that the creator of the IList produces a fixed amount of the list
-- and inserts a Fork constructor containing another Par computation that will
-- produce more of the list. The consumer, upon finding a Fork, calls fork to
-- start production of the next chunk of the list. The Fork does not have to be
-- at the end; for example, the list might be produced in chunks of 200
-- elements, with the first Fork being at the 100 element mark, and very 200
-- elements thereafter. This would mean that at any time there would be at
-- least 100 and yp to 300 elements waiting to be consumed.
--
module RateLimitingStream (
  Stream(..),
  foldS,
  streamFromList
                          ) where

import System.Environment
import Control.Monad.IO.Class
import Control.Monad.Par
import Control.DeepSeq
import GHC.Generics (Generic)

data IList a = Nil
             | Cons a (IVar (IList a))
             | Fork (Par ()) (IList a) deriving (Generic)

type Stream a = IVar (IList a)

-- NFData defines functions which allows me/us to define how to evaluate 
-- the data values i/we have so that it reduces to "normal form".
--
instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork p a) = seq p (rnf a)

streamFromList :: NFData a => Int -> [a] -> Par (Stream a)
streamFromList n xs = do
  v <- new
  fork $ consume n n xs v
  return v
    where 
      consume :: NFData a => Int -> Int -> [a] -> IVar (IList a) -> Par ()
      consume _ _ []     var = put var Nil
      consume 0 n (x:xs) var = do
        tail <- new
        let parFn = consume n n xs tail -- par function created when the counter reaches 0 => new chunk is needed.
        put var (Fork parFn (Cons x tail))
      consume counter n (x:xs) var = do
        tail <- new
        put var (Cons x tail) -- builds the list as "counter /= 0"
        consume (counter - 1) n xs tail

-- folding operation on a Stream that is parallelizable, what will happen is
-- that the folding proceeds as usual till the point "foldS" meets a "Fork"
-- which will fork a parallel computation.
foldS :: (a -> b -> a) -> a -> Stream b -> Par a
foldS fn !acc inS = do
  ilist <- get inS
  case ilist of
      Nil -> return acc
      Fork op (Cons h t) -> do
        fork op
        foldS fn (fn acc h) t
      Cons h t -> foldS fn (fn acc h) t

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
            Fork op (Cons h t) -> fork op >> do
              newtail <- new 
              put outS (Cons (fn h) newtail)
              consume t newtail
            Cons h t -> do
              newtail <- new
              put outS (Cons (fn h) newtail)
              consume t newtail

-- Filtering applies a predicate to the Stream and builds a Stream
-- containing all those elements that the predicate likes while discarding
-- those elements which failed the predicates.
--
filterS :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
filterS f inS  = do
  outS <- new
  fork $ consume inS outS
  return outS
    where
      consume inS outS = do
        ilist <- get inS
        case ilist of
            Nil -> put outS Nil
            Fork op (Cons h t) ->
              fork op >>
                if f h then
                       do
                         tail <- new 
                         put outS (Cons h tail)
                         consume t tail
                         else consume t outS
            Cons h t
              | f h -> do
                tl <- new
                put outS (Cons h tl)
                consume t tl
              | otherwise -> do
                consume t outS


allLTOne :: Par (Stream Int)
allLTOne =
  let xs = streamFromList 10 [1..1000]
  in filterS (<1) (runPar xs)

allBetween101and201 :: Par (Stream Int)
allBetween101and201 =
  let xs = streamFromList 10 [1..1000]
  in filterS (\e -> (e>101) && (e<201)) (runPar xs)

test :: Int -> Int -> Par Int
test chunkSize eleS =
  let es = [1..eleS]
      xs = streamFromList chunkSize es
      ys = mapS (+1) (runPar xs)
  in foldS (+) 0 (runPar ys)

main :: IO ()
main = do
  [chunkSize, elementsToGen] <- fmap (fmap read) getArgs
  sum <- runParIO (test chunkSize elementsToGen)
  putStrLn $ "The sum is : " ++ show sum
  result <- runParIO allLTOne
  case (runPar $ get result) of
      Nil -> putStrLn "No elements LT 1 found."
      _ -> putStrLn $ "Elements LT 1 found. "
  result2 <- runParIO allBetween101and201
  case (runPar $ get result2) of
      Nil -> putStrLn "No elements > 101 and < 201 found."
      _ -> putStrLn $ "Elements > 101 and < 201 found. "



