
import Control.Monad.Par
import Control.DeepSeq
import System.Environment

fib :: Int -> Int
fib n = f n 1
  where f 0 acc = acc
        f n acc = f (n-1) (n * acc)

-- Prevent clashing with the same name in 'Control.Monad.Par'
parMapM' :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM' f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

-- If you don't want to wait for the results, use this instead.
parMapMM :: NFData b => (a -> Par b) -> [a] -> Par [IVar b]
parMapMM f as = mapM (spawn . f) as

sequential_test :: [String] -> IO Int
sequential_test args = do
  return $ fib (head $ map (read :: String -> Int) args)

parallel_test_1 :: [String] -> IO [Int]
parallel_test_1 args = do
  return $ runPar $ parMapM' f $ map (read:: String -> Int) args
    where f = \a -> do
            x <- new
            fork (put x (fib a))
            _a <- get x
            return _a

-- parMapM and the like function parMapM' here waits for results to complete
-- before returning. Depending on the context, this may or may not be the most
-- useful behavior. If you don't want to wait for the results, then you could
-- always just use mapM (spawn . f) which returns a list of IVars
main :: IO ()
main = do
  args <- getArgs
  answer <- parallel_test_1 args
  -- answer <- sequential_test args
  putStrLn ("The result is: " ++ show (answer))


