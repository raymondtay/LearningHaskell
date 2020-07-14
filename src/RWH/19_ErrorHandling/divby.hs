{-# LANGUAGE FlexibleContexts #-}

import Control.Exception
import Control.Monad (liftM, mapM)
import System.IO

-- Note:
-- In this chapter, there's been a change to the way how Either works in the
-- latest GHCi / GHC; that is we can no longer except that "fail blah blah"
-- will auto convert to a Left(blah blah) as shown in RWH
--
-- After examining the code here, i find the abstraction of delaying the Monad
-- m in which to run `divByGeneric` in to be a very nice way.
--
divBy :: Integral a => a -> [a] -> Maybe [a]
divBy = divByGeneric -- or divByM works too!

divByM :: Integral a => a -> [a] -> Maybe [a]
divByM numerator denominators =
    mapM (numerator `safeDiv`) denominators
    where safeDiv _ 0 = Nothing
          safeDiv x y = Just $ x `div` y -- or return $ x `div` y works.


divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0: _) = fail "division by zero in divByGeneric"
divByGeneric numerator (denom : xs) =
  do next <- divByGeneric numerator xs
     return ((numerator `div` denom) : next)

divT :: String -> Maybe [Int]
divT s = divBy n m
  where n = read (head $ words s)
        m = map read (tail $ words s)

liftDivT :: IO String -> IO (Maybe [Int])
liftDivT = liftM divT

main :: IO ()
main = do
  putStrLn "Enter two (2) numbers"
  answer <- getLine `catch` (\e -> do let err = show (e :: IOException)
                                      hPutStr stderr ("Error: unable to read anything: " ++ show e)
                                      return "1 1")
  putStrLn ("You gave me: " ++ show answer)
  putStrLn ("The answer is " ++ case divT answer of Nothing -> "nothing"; Just v -> show v)
  return ()

