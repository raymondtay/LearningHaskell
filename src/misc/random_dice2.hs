module RandomExample2 where

--
-- The improved system from random-dice.hs
--
import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import qualified Data.DList                as DL
import           System.Random

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

--
-- To run:
-- $> runState intToDie' $ mkStdGen 34
--
intToDie' :: State StdGen Die
intToDie' = intToDie <$> state (randomR (1, 6))

--
-- This will generate the same result each and every time
-- but its ok for the purposes of this demonstration.
-- randomR returns
-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
-- so it really is a pair where 1st ele of pair is a random num generator
-- and the 2nd ele of pair is the value generated.
--
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR(1,6) s
      (d2, s2) = randomR(1,6) s1
      (d3, _)  = randomR(1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

--
-- To run:
-- $> runState rollDieThreeTimes' $ mkStdGen 34
--
rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

--
-- To run:
-- $> runState infiniteDie $ mkStdGen 44
--
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

{-
  *RandomExample2> execState  (nDie 4) (mkStdGen 1)
  879767458 1872071452
  *RandomExample2> evalState  (nDie 4) (mkStdGen 1)
  [DieSix,DieFive,DieTwo,DieSix]
  *RandomExample2> evalState  (nDie 4) (mkStdGen 2)
  [DieSix,DieFour,DieOne,DieFive]
  *RandomExample2> evalState  (nDie 4) (mkStdGen 4)
  [DieSix,DieThree,DieThree,DieTwo]
-}

--
-- Tells you how many rolls it took
-- to reach the number 20.
-- Constraints Programming ?
--
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR(1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsToGetN :: StdGen -> Int -> Int
rollsToGetN g ceil = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= ceil = count
          | otherwise =
            let (die, nextGen) = randomR(1, 6) gen
            in go (sum + die) (count + 1) nextGen

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n


fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
  let dlist = execState (mapM_ addResult' list) DL.empty
  in DL.apply dlist []

addResult' :: Integer -> State (DL.DList String)()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)


addResult :: Integer -> State [String]()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)


main :: IO ()
main =
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
