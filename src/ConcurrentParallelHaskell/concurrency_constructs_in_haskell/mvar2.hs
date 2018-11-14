import Control.Concurrent

noblock = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x' ; putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

blocking = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'
  forkIO $ do putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

-- If you run the `blocking` version, you will see immediately infact 
-- that the runtime will refuse to execute the program and issues a
-- error message.
--
-- main = noblock
main = blocking
