{-# LANGUAGE DeriveDataTypeable #-}
import Control.Concurrent
import Data.Unique
import Control.Exception
import Data.Typeable


-- The general idea is to fork a new thread that will wait for t microseconds
-- and then call throwTo to throw the Timeout exception back to the original
-- thread; that much seems straightforward enough. If the operation completes
-- within the time limit, then we must ensure that this thread never throws its
-- Timeout exception, so timeout must kill the thread before returning.
--


data Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show (Timeout _) = "timeout"

instance Exception Timeout

-- <<timeout-sig
timeout :: Int -> IO a -> IO (Maybe a)
-- >>

-- <<timeout
timeout t m
    | t <  0    = fmap Just m                           -- <1>
    | t == 0    = return Nothing                        -- <1>
    | otherwise = do
        pid <- myThreadId                               -- <2>
        u <- newUnique                                  -- <3>
        let ex = Timeout u                              -- <3>
        handleJust                                      -- <4>
           (\e -> if e == ex then Just () else Nothing) -- <5>
           (\_ -> return Nothing)                       -- <6>
           (bracket (forkIO $ do threadDelay t          -- <7>
                                 throwTo pid ex)
                    (\tid -> throwTo tid ThreadKilled)  -- <8>
                    (\_ -> fmap Just m))                -- <9>
-- >>

main = (timeout 200000 $ timeout 100000 $ timeout 300000 $ threadDelay 1000000) >>= print

-- Tricky situation to consider: What happens if both the child thread and the
-- parent thread try to call throwTo at the same time? Who wins?
--
-- The answer depends on the semantics of throwTo. In order for this
-- implementation of timeout to work properly, the call to bracket must not be
-- able to return while the Timeout exception can still be thrown; otherwise,
-- the exception can leak. Hence, the call to throwTo that kills the child
-- thread must be synchronous. Once this call returns. the child thread cannot
-- throw its exception anymore. Indeed, this guarantee is provided by the
-- semantics of throwTo. A call to throwTo returns only after the exception has
-- been raised in the target thread.
--
