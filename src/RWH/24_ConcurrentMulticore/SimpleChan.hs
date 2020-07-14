import Control.Concurrent
import Control.Monad.IO.Class

-- simple demonstration of a Control.Concurrent.Chan
-- which is supposedly constructed from MVars
main = do
  c <- newChan
  _ <- writeChan c 1
  liftIO $ putStrLn ("Written a value to the channel")
  r <- readChan c
  putStrLn ("Saw a value from the channel: " ++ show r)

