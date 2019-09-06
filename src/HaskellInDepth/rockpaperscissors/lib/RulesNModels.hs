
module RulesNModels where

import Data.List (group, sort)
import Control.Monad (replicateM)
import Control.Monad.State
import System.Random

data Weapon = Rock | Paper | Scissors deriving (Show, Bounded, Enum, Eq, Ord)

data Winner = First | Second | Draw deriving (Show, Eq, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (a, b) = if a == b then Draw else Second

instance Random Weapon where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (r, g') -> (toEnum r, g')
  random g = randomR (minBound, maxBound) g

-- generate weapons rather randomly
randomWeapon :: State StdGen Weapon
randomWeapon = state random 

-- 1-game round and get a pair of weapons, randomly by design but that details
-- is unknown to the fellas here.
gameRound :: State StdGen (Weapon, Weapon)
gameRound = (,) <$> randomWeapon <*> randomWeapon

game :: Int -> State StdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where
    counts xs = map (\xs@(x:_) -> (x, length xs)) $ group $ sort xs



