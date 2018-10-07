{-|
   GHC's runtime system treats the program's original thread of control differently from
   other threads. When this thread finishes executing, the runtime system considers the program
   as a whole to have completed. If any other threads are executing at the time, they are
   terminated.

   As a result, when we have long running threads that must not be killed
   we need to make special arrangements to ensure the main thread doesn't complete until
   the others do.
-}
 module NiceFork (
 ThreadManager, 
 newManager,
 forkManaged,
 getStatus,
 waitFor,
 waitAll
                 ) where
import Control.Monad (join)
import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished
                  | Threw Exception
                  deriving (Eq, Show)

-- | Create a new thread manager.
newManager :: IO ThreadManager

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId

-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until a specific managed thread terminates
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until all managed threads terminate.
waitAll :: ThreadManager -> IO ()

-- we maintain a map from thread id to thread state i.e. a thread map
newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus))) deriving (Eq)
-- for reach thread that we manage, we maintain a MVar. A per-thread MVar
-- starts off empty, which indicates that the thread is executing. When the
-- thread finishes or is killed by an uncaught exception we put this
-- information into the MVar.
newManager = (<$>) Mgr (newMVar M.empty)

forkManaged (Mgr mgr) body =
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state (either Threw (const Finished) result)
    return (M.insert tid state m, tid)

-- safely modifying an mvar
--
-- When we use `modifyMVar` instead of manually managing an MVar with takeMVar
-- and putMVar, we avoid two common kinds of concurrency bugs:
-- + Forgetting to put a value back into an MVar. This can result in deadlock,
--   in which some thread waits forever on an MVar that will never have a value
--   put into it.
-- + Failure to account for the possiblity that an exception might be thrown,
--   disrupting the flow of a piece of code. This can result in a call to putMVar
--   that should occur, but does not actually happen, again leading to deadlock.
--
-- Because of these nice safety properties, it is very wise indeed to use
-- `modifyMVar` whenever possible.
--


getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m -> 
    case M.lookup tid m of
        Nothing -> return (m, Nothing)
        Just st -> tryTakeMVar st >>= \mst -> case mst of
                                                  Nothing -> return (m, Just Running)
                                                  Just sth -> return (M.delete tid m, Just sth)

-- If the thread is no longer being managed (or never managed), it returns
-- Nothing.
-- If the thread is still running, returns Just Running. Otherwise, it
-- indicates why the thread terminated and stops managing the thread. If teh
-- tryTakeMVar funciton finds that the MVar is empty, it returns Nothing
-- immediately instead of blocking.
--

-- `waitFor` blocks until the given thread terminates before returning.
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
                 (Nothing, _) -> (m, Nothing)
                 (done, m') -> (m', done)
  case maybeDone of
      Nothing -> return Nothing
      Just st -> (<$>) Just (takeMVar st)

-- If there is a value to extract, we take the thread's exit status from the
-- MVar and return it. Our final useful function simply waits for all
-- currently managed threads to complete and ignores their exit statuses:
--

waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where elems m = return (M.empty, M.elems m)


-- Writing tigher code
--
-- Our definition of waitFor is a little unsatisfactory, because we are
-- performing more or less the same case analysis in two places: inside the
-- function called by modifyMVar and again on its return value.
--
--

waitFor2 (Mgr mgr) tid =
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
                 (Nothing, _) -> (m, return Nothing)
                 (Just st, m') -> (m', fmap Just (takeMVar st))


