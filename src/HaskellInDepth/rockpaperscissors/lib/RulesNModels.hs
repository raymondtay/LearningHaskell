
module RulesNModels () where

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


