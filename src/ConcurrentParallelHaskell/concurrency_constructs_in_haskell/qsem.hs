import Control.Concurrent

-- simple quantity semaphores using a binary semaphore approach. 
-- the pattern is simple to understand, ie before we attempt to perform
-- a computation, we will wait for the binary semaphore to be available and
-- then proceed to compute it. Once done, we will signal the others.
--
-- nice thing about this approach is that the function `task` leverages on the
-- passed-in semaphore and operates on it, needing nothing else. Of course,
-- this is a simple example but the general idea does not deviate.
task sem = do
  _ <- waitQSem sem
  _ <- forkIO $ putStrLn "yes\n"
  _ <- signalQSem sem
  return ()


main = do
  sem <- newQSem 1
  _ <- forkIO $ task sem
  _ <- forkIO $ task sem
  _ <- forkIO $ task sem
  return ()


