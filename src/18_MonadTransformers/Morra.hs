import Data.Char
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import System.Exit


--
-- Write the game Morra using StateT and IO. The state being accumulated is the
-- score of theplayer and the computer AI the player is playing against. To
-- start, make the computer choose its play randomly.
-- On exit, report the scores for the player and the computer , congratulating
-- the winner.
--
-- What is this game? Looking up the wikipedia reveals this explanation :
--
-- > Morra is a hand game that dates back thousands of years to ancient Roman and
-- > Greek times. Each player simultaneously reveals their hand, extending any
-- > number of fingers, and calls out a number. Any player who successfully
-- > guesses the total number of fingers revealed by all players combined scores a point.
--
-- Add a human vs. human mode to the game with interstitial screens between
-- input prompts so the players can change out of the hotseat without seeing
-- the other player's answer.
--
-- Improve the computer AI slightly by making it remember 3-grams of the
-- players behavior, adjusting its answer instead of deciding randomly when the
-- player's behavior matches a known behavior.
--

-- 
-- The purpose of designing this game is really to exercise my familiarity with
-- the Monad Transformers with the end goal of gaining proficieny and like any programming journey
-- the important thing to keep in focus is to practice ... practice .. practice
-- and keep practicing till the crux is revealed. How do i know its been
-- revealed ? Its simple : if i can explain why the code works and convince
-- myself thoroughly then i can truly say that i have completely understood the
-- program.
--
--

data GameMode = Even | Odd deriving (Show, Eq)

-- 
-- Generates a random odd number between 1 - 10
--
genOdd  :: IO Integer
genOdd = do
  x <- getStdRandom (randomR (1, 10))
  case (odd x) of
    True -> return x
    False -> genOdd

-- 
-- Generates a random even number between 1 - 10
--
genEven  :: IO Integer
genEven = do
  x <- getStdRandom (randomR (1, 10))
  case (even x) of
    True -> return x
    False -> genEven

--
-- Represents the non-computer player and there's a bit of code duplication
-- which i'll take care of a little later.
--
play :: StateT GameMode IO Integer
play = StateT (\s -> case s of
                       Odd  -> (liftM (,) $ genOdd) <*> (return s)
                       Even -> (liftM (,) $ genEven) <*> (return s))
-- 
-- Computer looks at all the past moves of the player and attempts to guess,
-- poorly, what this number is going to be ...
--
computerPlay :: [Integer] -> StateT GameMode IO Integer
computerPlay moves = StateT (\s -> case s of
  Odd  -> (liftM (,) $ genOdd) <*> (return s)
  Even -> (liftM (,) $ findNext Even moves) <*> (return s))


-- 
-- Find the next move the computer needs to make
--
findNext :: GameMode -> [Integer] -> IO Integer
findNext _ [] = genEven
findNext mode (x:xs)
  | mode == Odd && (odd x) = return x
  | mode == Even && (even x) = return x
  | otherwise = findNext mode xs


-- Note: The ghc 8.8.3 compiler needed help to indicate that `IO Int` is
-- necessary else compile-time error. Unfortunately, my understanding of
-- Haskell is not the level i can explained comfortably why this type
-- annotation was necessary ... nonetheless, it doesn't stop me from trying!
--
decideGameMode :: IO GameMode
decideGameMode = 
  (getStdRandom (randomR (1,2)) :: IO Int) >>= (\r -> case r of 1 -> return Odd; _ -> return Even)

-- Records what number did the human player made during his/her turn
--
recordPlayerMove :: Monad m => Integer -> StateT [Integer] m Integer
recordPlayerMove move = do 
  moves <- get
  put (move : moves)
  return move

-- Playing the actual game and the caveat is that the "player" does not have
-- state i.e. it does not remember anything.
playGame :: [Integer] -> IO ()
playGame accum = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- C is Evens, P is Odd"
  hS <- liftM fst (runStateT play $ Odd )
  cS <- liftM fst (runStateT play $ Even)
  putStrLn $ "P: " ++ show hS
  putStrLn $ "C: " ++ show cS
  putStrLn $ case cS == hS of True -> "C wins!"; False -> "P wins!"
  putChar '\n'
  return ()

-- Playing the actual game and the sweet spot here is that the "player" does
-- HAVE state and the strategy i've taken with this works but i wonder if
-- there's an area to be improved ...
--
-- In any case, the dissection of this implementation is leveraging the Reader
-- monad which is embedded into the IO monad
--
playGameS :: ReaderT [Integer] IO [Integer] 
playGameS = do
  accum <- ask
  liftIO $ putStrLn "-- P is Player"
  liftIO $ putStrLn "-- C is Computer"
  liftIO $ putStrLn "-- C is Evens, P is Odd"
  -- liftIO $ putStrLn . show $ accum -- debugging purposes only 
  (playerMove  , playerMoves) <- liftIO $ liftM fst (decideGameMode >>= runStateT play) >>= (\x -> runStateT (recordPlayerMove x) accum)
  (computerMove, _          ) <- liftIO $ liftM fst (runStateT (computerPlay playerMoves) $ Even ) >>= (\x -> runStateT (recordPlayerMove x) accum)
  liftIO . putStrLn $ "P: " ++ show playerMove
  liftIO . putStrLn $ "C: " ++ show computerMove
  liftIO . putStrLn $ case playerMove == computerMove of True -> "C wins!"; False -> "P wins!"
  return playerMoves

--
-- the function should be self descriptive, no? 
--
repeatTillQuit :: Char -> IO () -> [Integer] -> IO ()
repeatTillQuit killCode action accum =
  if killCode == 'q' then do
                     die "Quitting..."
                     else do 
                       action
                       putStrLn "Press 'q' to quit, 'c' to continue..."
                       kc <- getChar
                       putChar '\n'
                       repeatTillQuit kc action accum

--
-- the function should be self descriptive, no? there's a slight twist to it
-- and the clue can be found by comparing their differences.
--
repeatTillQuit2 :: Char -> (ReaderT [Integer] IO [Integer]) -> [Integer] -> IO ()
repeatTillQuit2 killCode action accum =
  if killCode == 'q' then do
                     die "Quitting..."
                     else do 
                       accum2 <- (runReaderT action $ accum)
                       putStrLn "Press 'q' to quit, 'c' to continue..."
                       kc <- getChar
                       putChar '\n'
                       repeatTillQuit2 kc action accum2


main :: IO ()
main = do
  putStrLn "\n++ ☺ Welcome to the game ++\n"
  let initS = [] :: [Integer]
  repeatTillQuit2 'c' playGameS initS
  -- repeatTillQuit 'c' (playGame initS) initS


