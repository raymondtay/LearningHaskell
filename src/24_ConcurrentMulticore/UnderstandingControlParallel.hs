import Control.Parallel
import Control.Concurrent

sleepFor :: Int -> IO ()
sleepFor time = threadDelay (time * 1000000)

result :: IO ()
result = do {forkIO (newEmptyMVar >>= \new_hole -> do { (sleepFor 5) >> putMVar new_hole 1 >> putStrLn "LHS: I am here!"}); return ()} `par` (do { putStrLn "RHS: I am here!" })

result2 :: IO ()
result2 = do {forkIO (newEmptyMVar >>= \new_hole -> do { (sleepFor 5) >> putMVar new_hole 1 >> putStrLn "Here!"}) ; return () }

result3 :: IO ()
result3 = do {forkIO (newEmptyMVar >>= \new_hole -> do { (sleepFor 5) >> putMVar new_hole 1 >> putStrLn "LHS: I am here!"}); return ()} `pseq` (do { putStrLn "RHS: I am here!" })

-- when "result2" was executed, it was obvious that this runs immediately 
-- but when paired with "par" then the RHS is ever executed only and there's no
-- visible effect from the "lhs" of the computation.
--
-- ghc -c UnderstandingControlParallel.hs
-- ghc -threaded -o UnderstandingControlParallel UnderstandingControlParallel.o
-- ./UnderstandingControlParallel +RTS -N4 -RTS foo

main :: IO ()
main = do
  result3

