module RandomExample2 where

-- 
-- The improved system from random-dice.hs
--
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

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

