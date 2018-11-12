import Control.Concurrent
import Text.Printf
import Control.Monad

main = loop
  where
    loop = do
      s <- getLine
      if s == "exit"
          then return ()
          else do forkIO $ setReminder s
                  loop

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, i'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)
  printf "%d seconds is up! BING! \BEL\n" t

-- If you were to run this program and notice its behavior, you can arrive at
-- the conclusion that : the program terminates when main returns, even if
-- there are other threads still running. The other threads simply stop running
-- and cease to exist after main returns.
--
