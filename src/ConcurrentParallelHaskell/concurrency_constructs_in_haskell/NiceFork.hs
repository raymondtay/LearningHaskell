module NiceFork (
  ThreadManager,
  newManager,
  forkManaged,
  getStatus,
  waitFor,
  waitAll
  ) where

import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data MyException = MyException deriving Show
instance Exception MyException
instance Eq MyException where
  (==) MyException MyException = True -- seems a reasonable definition.

data ThreadStatus = Running | Finished | Threw MyException deriving (Show, Eq)

-- | Manage thread IDs to a thread state; thread statuses is held in a MVar so
-- | that all threads will get a consistent view of the manager's state.
newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus))) deriving (Eq)

-- | Create a new thread manager
newManager :: IO ThreadManager
newManager = (<$>) Mgr (newMVar M.empty)

-- | Create a new managed thread
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) action = 
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid   <- forkIO $ do
      result <- try action
      putMVar state (either Threw (const Finished) result)
    return (M.insert tid state m, tid)

-- | Immediately return the status of a managed thread
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
        Nothing -> return (m, Nothing)
        Just st -> tryTakeMVar st >>= \mst -> case mst of
                                                  Nothing -> return (m, Just Running)
                                                  Just sth -> return (M.delete tid m, Just sth)

-- | Block until a specific managed thread terminates
-- | 
-- | notes:
-- | this function (\_ _ -> Nothing) basically does 2 actions: (a) removes the
-- | the found value from the map; (b) returns the found value.
--
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
                 (Nothing, _ ) -> (m , Nothing)
                 (done,    m') -> (m', done)
  case maybeDone of 
      Nothing -> return Nothing
      Just st -> (<$>) Just (takeMVar st)

-- | Block untill all managed threads terminate.
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where elems m = return (M.empty, M.elems m)



