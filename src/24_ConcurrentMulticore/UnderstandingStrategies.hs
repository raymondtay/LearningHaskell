
module AboutStrategies where

import Control.Parallel
import Control.Parallel.Strategies

-- Run: runEval $ simple (1, 2) OR equivalently in applicative-style:
-- (,) <$> rpar 1 <*> rpar 2
--
-- Run: runEval $ simple ([undefined], 2)
-- should vomit the callstack since Eval is a strict Monad and again in
-- applicative-style:
-- (,) <$> rpar [undefined] <*> rpar 2
--
simple :: Strategy (a, b)
simple (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

-- Run: runEval $ composition (1,2)
composition = simple `dot` simple -- equivalent to simple `dot` simple `dot` r0; r0 is a no-op.

