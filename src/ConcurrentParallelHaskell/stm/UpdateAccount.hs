import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

main :: IO ()
main = do v <- newMVar 10000
          s <- newMVar [("a", 8)]
          forkDelay 4 $ updateMoneyAndStock "a" 1000 v s
          forkDelay 4 $ printMoneyAndStock v s
          _ <- getLine -- wait for completion
          return ()

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a, Integer)] -> IO ()
updateMoneyAndStock product price money stock =
  do s <- takeMVar stock
     let Just productNo = lookup product s
     if productNo > 0
        then do m <- takeMVar money
                let newS = map (\(k,v) -> if k == product then (k, v -1) else (k,v)) s
                putMVar money (m + price) >> putMVar stock newS
        else putMVar stock s


forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 1000000)

printMoneyAndStock :: Show a => MVar Integer -> MVar [(a, Integer)] -> IO ()
printMoneyAndStock money stock = do m <- readMVar money
                                    s <- readMVar stock
                                    putStrLn $ show m ++ "\n" ++ show s


