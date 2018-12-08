{-# LANGUAGE AllowAmbiguousTypes #-}

-- Understanding the basics of masking and what it means to mask
--
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class

data DummyException = DummyException deriving Show
instance Exception DummyException

throwErrorTo :: ThreadId -> IO ()
throwErrorTo target = do
  throwTo target DummyException

problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = mask $ \restore -> do
  a <- takeMVar m
  r <- restore (f a) `catch` (\e -> do putMVar m a; liftIO (putStrLn "Caught!"); throw (e :: DummyException))
  putMVar m r

--
-- For demonstration purposes, there are two instances of the function where
-- in <1>, it is clear that no known exception or unexpected error is caught
-- and hence never thrown.
-- In <2> , the situation is a little more complex in that an obvious exception
-- is thrown with every deliberate intention of showcasing what does "masking"
-- actually do.
--
--
-- There is a subtle end to this story and that is the fact that the exception
-- can be caught when `(f a)` is actively executing and the presumption here is
-- that `takeMVar` will not take forever to execute which we know from the
-- documentation that its not guaranteed.
--
main = do
  target <- forkIO (putStrLn "AH?")
  _ <- forkIO (throwErrorTo target)
  m <- newEmptyMVar
  n <- newEmptyMVar
  putMVar m 1
  _ <- forkIO (problem m (\x -> do liftIO (putStrLn $ "Saw the value: " ++ show x); return (x+1)))         -- <1>
  putMVar n 2
  _ <- forkIO (problem n (\x -> do liftIO (putStrLn $ "Saw the value: " ++ show x); throw DummyException)) -- <2>
  return ()

-- When you really need to call an interruptible function but cannot afford the
-- possibility that an asynchronous exception might be raised, there is a last
-- resort:
--
-- uninterruptibleMask :: ((IO a -> IO a) -> IO b) -> IO b
--
-- This works just like mask, except that interruptible operations may not
-- receive asynchronous exceptions. Be very careful with uninterruptibleMask;
-- accidental misuse may leave your application unresponsive. Every instance of
-- uninterruptibleMask should be treated with utmost suspicion.
--
--


