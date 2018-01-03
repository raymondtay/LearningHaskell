module MyAsync where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM

-- STM's orElse allows us to defined `waitEither` much more efficiently.
-- Furthermore, the extra flexibility of STM lets us compose Asyncs together in
-- more interesting ways. But first, we need to rewrite the Async
-- implementation in terms of STM, rather than MVar. The translation is
-- straight forwardw: we just replace MVar with TMVar
--

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  tid <- forkFinally action (atomically . putTMVar var) -- wrap the call to putTMVar in the child thread.
  return (Async tid var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
      Left e -> throwSTM e
      Right a -> return a

-- define waitEither by composing two calls to waitSTM using orElse....
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a) `orElse` fmap Right (waitSTM b)

-- more generally, we can wait for any number of async simultaneously. The
-- function waitAny does this by first mapping waitSTM over a list of Asyncs
-- and then composing the calls together by folding them with orElse:
--
waitAny :: [Async a] -> IO a
waitAny xs = atomically $ foldr orElse retry $ map waitSTM xs

