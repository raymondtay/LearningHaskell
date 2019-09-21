module Polish where

import Control.Monad.State

type Stack = [Integer]

type EvalM = State Stack

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
  stack <- get
  put (tail stack)
  pure (head stack)

-- Buggy with no error / fault recovery.
-- TODO: fix !
evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
  where 
    evalRPN' = traverse step (words expr) >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step t = push (read t)
    processTops op = op <$> pop <*> pop >>= push


