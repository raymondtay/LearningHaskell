import Control.Concurrent

nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    yield -- force this thread to temporarily yield the CPU
    modifyMVar_ inner $ \y -> return (y + 1)
    return (x + 1)
  putStrLn "done"

-- if we run this in the GHCi, it will usually (though not always) print
-- nothing, indicating that both threads got stuck.
--
main = do
  a <- newMVar 1
  b <- newMVar 2
  forkIO $ nestedModification a b
  forkIO $ nestedModification b a



-- Across languages, the usual way to solve an order inversion problem is to
-- always follow a consistent order when acquiring resources. Since this
-- approach requires manual adherence to a coding convention, it is easy to
-- miss in practice.
--



--
