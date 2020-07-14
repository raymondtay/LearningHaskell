import Control.Concurrent
import Control.Monad.IO.Class

grab :: QSem -> IO ()
grab s =
  waitQSem s >>
  putStrLn "Got it!" >>
  threadDelay 2000000 >> -- sleep for 2 seconds
  signalQSem s

main :: IO ()
main = do
  sem <- newQSem 1 -- binary semaphore
  tid1 <- forkIO $ grab sem
  tid2 <- forkIO $ grab sem
  tid3 <- forkIO $ grab sem
  _ <- threadDelay 5000000 -- sleep for 5 seconds
  putStrLn "we are done."


