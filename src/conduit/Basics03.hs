import Conduit

magic :: Int -> IO Int
magic x = do
  putStrLn $ "I'm doing magic with " ++ show x
  return $ x * 2

-- This version is largely different from Basics02.hs from the perspective that
-- we have had to re-implement the behavior of takeWhile, mapM, and mapM_
-- ourselves and the solution is less compositional. Conduit makes it easuy to
-- get the right behavior: interleaved effects, constant memory, and
-- deterministic resource usage.
--
main :: IO ()
main = do
  let go [] = return ()
      go (x:xs) = do
        y <- magic x
        if y < 18
            then do
              print y
              go xs
              else return ()

  go $ take 10 [1..]

