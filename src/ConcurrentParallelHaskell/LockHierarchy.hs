
import Control.Concurrent

-- this problem is known as the [lock order inversion] problem.
-- across languages, the usual way to solver an order inversion problem 
-- is to always follow a consistent order when acquiring resourcess. Since this approach
-- requires manual adherence to a coding conventions, it is easy to miss in practice.
--
nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    yield -- force this thread to temporarily yield the CPU
    modifyMVar_ inner $ \y -> return (y+1)
    return (x+1)
  putStrLn "done"

main = do
  a <- newMVar 1
  b <- newMVar 2
  forkIO $ nestedModification a b
  forkIO $ nestedModification b a

-- Concurrent software is also prone to starvation, in which one thread hogs a shread resource, preventing another from using it. It's easy to imagine how this might occur : one thread calls modifyMVar with a body that executess for 100 milliseconds
-- while another calls modifyMVar on the same MVar with a body that executes for 1 milliseconds.
-- The second thread cannot make progress until the first puts a value back into the MVar.
-- The nonstrict nature of the MVar type can either cause or exacerbate a starvation problem. If we put a thunk
-- into an MVar that will be expensive to evaluate, and then take it out of the MVar in a thread that otherwise looks
-- like it ought to be cheap, that thread could suddenly become computationally expensive if it has to evaluate 
-- the chunk. This makes the advice we gave in that MVar and Chan are non-strict whic is particularly relevant.
--
--

