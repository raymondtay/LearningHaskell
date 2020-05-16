{-# LANGUAGE InstanceSigs #-}

import Data.Char
import System.Random
import Control.Monad.Reader
import Control.Monad.State

-- Morra game apparently is played in many places but i've never played this
-- game before. The version of Morra
--

data GameMode = Even | Odd deriving (Show)

player :: StateT s IO Int
player = StateT (\s -> do { c <- getChar; putStrLn . show $ c ; return (ord c, s)})

-- Depending on the player's GameMode, it would generate the appropriate number
-- and return the numerical results
play :: GameMode -> IO Int
play = (\s -> case s of Odd -> genOdd; Even -> genEven)

play2 :: StateT GameMode IO Int
play2 = StateT (\s -> case s of
                        Odd  -> (liftM (,) $ genOdd) <*> (return s)
                        Even -> (liftM (,) $ genEven) <*> (return s))

genOdd  :: IO Int
genOdd = do
  x <- getStdRandom (randomR (1, 100))
  case (odd x) of
    True -> return x
    False -> genOdd

genEven  :: IO Int
genEven = do
  x <- getStdRandom (randomR (1, 100))
  case (even x) of
    True -> return x
    False -> genEven

-- Note: The ghc 8.8.3 compiler needed help to indicate that `IO Int` is
-- necessary else compile-time error.
decideGameMode :: IO GameMode
decideGameMode = 
  (getStdRandom (randomR (1,2)) :: IO Int) >>= (\r -> case r of 1 -> return Odd; _ -> return Even)


-- Records what number did the human player made during his/her turn
--
recordPlayerMove :: Integer -> State [Integer] Integer
recordPlayerMove move = do 
  moves <- get
  put (move : moves)
  return move


main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- C is Evens, P is Odd"
  putStrLn "Dear P, please enter: "
  mode <- decideGameMode
  c <- getChar
  putChar '\n'
  putStrLn $ "P : " ++ (show . digitToInt $ c)
  c1 <- liftM fst (runStateT play2 $ mode)
  putStrLn $ "C : " ++ show c1
  return ()





