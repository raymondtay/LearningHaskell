
module GameOfDice where


import Control.Monad (replicateM)
import Control.Monad.RWS
import System.Random

type Dice = Int
type DiceGame = RWS
                 (Int, Int) -- Reader (dice bounds)
                 [Dice]     -- Writer (a history of rolls)
                 StdGen     -- State (random generator)

dice :: DiceGame Dice
dice = do
  bounds <- ask
  stdGen <- get
  let (dice, stdGen') = randomR bounds stdGen
  put stdGen'
  tell [dice]
  pure dice

dice2 :: DiceGame Dice
dice2 = do
  bounds <- ask
  dice <- state (randomR bounds)
  tell [dice]
  pure dice


doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice2 <*> dice2

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice2

diceGame :: DiceGame (Dice, Dice)
diceGame = dice >> dices 5 >> replicateM 2 (dices 4) >> dices 10 >> doubleDice

