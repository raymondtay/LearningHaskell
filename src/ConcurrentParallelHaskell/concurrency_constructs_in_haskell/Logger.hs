
import Control.Concurrent

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
          Message msg -> do
            putStrLn msg
            loop
          Stop s -> do
            putStrLn "logger: stop"
            putMVar s ()
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s


main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "this"
  logMessage l "is"
  logMessage l "him."
  logMessage l "bye"
  logStop l


