
import Control.Parallel
import Control.Parallel.Strategies

-- A lesson in thinking in terms of applying Strategies
--
--
-- begin with a simple defintion
simpleMapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
simpleMapReduce mF rF = rF . map mF

-- next, incorporate the evaluation strategies
mapReduce
  :: Strategy b -- evaluation strategy for mapping
  -> (a -> b)   -- map function
  -> Strategy c -- evaluation strategy for reduction
  -> ([b] -> c) -- reduce function
  -> [a]        -- list to map over
  -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input = mapResult `pseq` reduceResult
  where mapResult = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat



