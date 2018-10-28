
module PlayingStrategies where

import Control.DeepSeq
import Control.Parallel.Strategies (rpar, rseq, rparWith, using, Strategy)

-- This strategy takes a data structure (in this case its a pair), creates some
-- parallelism using rpar, and then returns the same structure
--
parPair1  :: Strategy (a, b)
parPair1 (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

fib n =
  f n 1
    where
      f 0 acc = acc
      f n acc = let acc' = acc * n in (f (n-1) acc')


-- Avoiding the name clash with the "Strategies" package, renamed 'parPair' to
-- 'parPair1'; to run the program, one can do either of the following:
-- runEval $ parPair1 (f 1, f4)
-- (f 3, f5) `using` parPair1
--
-- Special note: The parPair1 strategy embodies a fixed policy: it always
-- evaluates the components of the pair in parallel, and always to weak head
-- normal form. If we wanted to do something different with a pair - fully
-- evaluate the components to normal form, for example - we would have to write
-- a completely new Strategy. A better way to factor things isto write a
-- parameterized Strategy, which takes as arguments the Strategies to apply to
-- the components of the data structure.
--

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

-- Example run: runEval $ evalPair rpar rpar (fib 4, fib 2)
-- (24, 2)
--


runEvalPair :: Strategy (a, b)
runEvalPair = evalPair rpar rpar
-- Example run : runEval $ runEvalPair (fib 3, fib 5)
-- (6, 120)
-- 


rdeepseq :: NFData a => Strategy a
rdeepseq x  = rseq (force x)

parPair2 :: Strategy a -> Strategy b -> Strategy (a, b)
parPair2 sa sb = evalPair (rparWith sa) (rparWith sb)
-- Example run: runEval $ parPair2 rdeepseq rdeepseq (fib 44, fib 555)
--



