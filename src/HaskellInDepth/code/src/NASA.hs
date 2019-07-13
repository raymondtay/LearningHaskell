{-# LANGUAGE InstanceSigs #-}

-- Do you dream of working for NASA ? Or do you prefer SpaceX and Blue Origin?
-- The above came from the book and i think its pretty tedious that the
-- developer has to repeat this effort for every haskell project.

data Direction = North | East | South | West deriving (Show)
data Turn = TNone | TLeft | TRight | TAround deriving (Show)

orient :: Turn -> Direction -> Direction
orient TNone d = d
orient TLeft North = West
orient TLeft South = East
orient TLeft East = North
orient TLeft West = South
orient TRight North = East
orient TRight East = South
orient TRight South = West
orient TRight West = North

findTurn :: Direction -> Direction -> Turn
findTurn = undefined

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

instance Eq Direction where
  (==) :: Direction -> Direction -> Bool
  North == North = True
  South == South = True
  West == West = True
  East == East = True
  _ == _ = False

instance Bounded Direction where
  minBound = North
  maxBound = South

instance Enum Direction where
  fromEnum :: Direction -> Int
  fromEnum North = 0
  fromEnum South = 3
  fromEnum East = 1
  fromEnum West = 2

  toEnum :: Int -> Direction
  toEnum 0 = North
  toEnum 3 = South
  toEnum 1 = East
  toEnum 2 = West

instance CyclicEnum Direction

