import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import System.Random

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


-- Instead of MVars, use TVars and TVars can be read and written as many times as desired.
--
updateMoneyAndStockStm :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStockStm product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
        then do m <- readTVar money
                let newS = map (\(k,v) -> if k == product then (k, v-1) else (k,v)) s
                writeTVar money (m + price ) >> writeTVar stock newS
     else return ()


-- A transaction is a computation guaranteed to be run reliably independent
-- from other transactions, and it always has a coherent view of the data. 
-- Transactions provide the illusion that a whole computation runs as an atomic
-- block inside the database and ensure that data maintains its integrity.
--
-- Transactions here is referring to the ACID model which ensures atomicity and
-- consistency after each transactions. Most SQL databases follow the ACID
-- model. Other database systems follow the BASE paradigm, which guarantees
-- eventual consistency instead.
--


-- 
-- Rolling back Transactions
--
-- The stm package not only brings the atomicity guarantees of transactions to
-- the Haskell world but also offers the ability to roll back some piece of
-- code. To signal that a transaction cannot continue, you nede to use the
-- "retry" function.
--



payByCard :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
payByCard product price money stock =
  do working <- isCardSystemWorking
     if not working
        then retry
        else updateMoneyAndStockStm product price money stock

isCardSystemWorking :: STM Bool
isCardSystemWorking = do
  a <- newTVar True
  readTVar a

hasEnoughCash :: STM Bool
hasEnoughCash = do
  a <- newTVar False
  readTVar a

--
-- While retry is a powerful tool, in some cases you may want to follow a path
-- different from waiting until the variables change and the invariants are
-- satisfied. For those occasions, "stm" provides the orElse combinatory
--

pay :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
pay product price money stock = 
  payByCard product price money stock `orElse` payByCash product price money stock

payByCash :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
payByCash product price money stock =
  do working <- hasEnoughCash
     if not working
        then retry
        else updateMoneyAndStockStm product price money stock


printMoneyAndStockStm :: Show a => TVar Integer -> TVar [(a, Integer)] -> IO ()
printMoneyAndStockStm money stock = do m <- readTVarIO money
                                       s <- readTVarIO stock
                                       putStrLn $ show m ++ "\n" ++ show s

main :: IO ()
main = do v <- newTVarIO 10000
          s <- newTVarIO [("a", 8)]
          forkDelay 4 $ atomically $ updateMoneyAndStockStm "a" 1000 v s
          forkDelay 4 $ atomically $ pay "a" 800 v s
          forkDelay 4 $ printMoneyAndStockStm v s
          _ <- getLine -- wait for completion
          return ()


