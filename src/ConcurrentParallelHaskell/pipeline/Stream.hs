{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveGeneric #-}

-- Here's how we can make use of parallelism between the stages of a pipeline.
-- Each stage of a imaginary pipeline is doing some computation on the stream
-- elements and maintaining state as it does so. When a pipeline stage
-- maintains some state, we cannot exploit parallelism between the stream
-- elements. Instead, we would like each of the pipeline stages to run on a
-- separate core, with the data streaming between them. The `Par` monad will
-- allow us to do exactly that.
-- Before that, we need some ADTs ready.
--

import Control.Monad.Par
import Control.DeepSeq
import GHC.Generics (Generic)

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

foldS :: (a -> b -> a) -> a -> Stream b -> Par a
foldS fn !acc instrm = do
  ilist <- get instrm
  case ilist of
      Nil -> return acc
      Cons h t -> foldS fn (fn acc h) t

