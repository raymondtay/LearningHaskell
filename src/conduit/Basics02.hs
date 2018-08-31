import Conduit

magic :: Int -> IO Int
magic x = do
  putStrLn $ "I'm doing magic with " ++ show x
  return $ x * 2

-- When you execute this program, you will notice a few things and there's a
-- break out (enabled via >>=) to allow us to have two different side-effecting
-- actions (mapM magic and mapM_ print). Meanwhile, in conduit, all we did was
-- erplace mapC (* 2) with mapMC magic. This is where we begin to see the
-- strenght of conduit : it allows us to build up largel pipelines of
-- components, and each of those components can be side-effecting.
--
main :: IO ()
main = do
  putStrLn "List version:"
  mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)
  putStrLn ""
  putStrLn "Conduit version:"
  runConduit $
    yieldMany [1..]
    .| takeC 10
    .| mapMC magic
    .| takeWhileC (< 18) 
    .| mapM_C print

