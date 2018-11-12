import Control.Concurrent

noblock = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r

blocking = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  r2 <- takeMVar m
  print r
  print r2

-- If you run the `blocking` version, you will see immediately infact 
-- that the runtime will refuse to execute the program and issues a
-- error message.
--
main = noblock
-- main = blocking
