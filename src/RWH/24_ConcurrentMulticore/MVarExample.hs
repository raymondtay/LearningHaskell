import Control.Concurrent

-- Semantics of MVar is that if an attempt is made to put a value into an MVar
-- that is already full, our thread is put to sleep untill another thread takes
-- the value out.
-- If you are coming from a background of concurrent programming in a
-- traditional language, you can think of an MVar as being useful for two
-- familiar purposes:
--  + Sending a message from one thread to another, for example, a
--    notififcation.
--  + Providing mutual exclusion for a piece of mutable data that is shared
--    among threads. We put the data into the MVar when it is not being used by
--    any thread. One thread then takes it out temporarily to read or modify
--    it.
--
communicate = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received: " ++ show v)
  putStrLn "sending"
  putMVar m "wake up!"


