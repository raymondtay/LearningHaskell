{-# LANGUAGE StandaloneDeriving,
             DeriveAnyClass
#-}

import Data.List    (nub, sort)


-- Do you dream of working for NASA ? Or do you prefer SpaceX and Blue Origin?
-- The above came from the book and i think its pretty tedious that the
-- developer has to repeat this effort for every haskell project.

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d


data Direction = North | East | South | West deriving (Eq, Bounded, Enum, CyclicEnum, Show)
data Turn = TNone | TLeft | TRight | TAround deriving (Show, Enum, Bounded, BoundedEnum)

-- Haskell, by 
orient :: Turn -> Direction -> Direction
orient TNone = id
orient TLeft = cpred
orient TRight = csucc
orient TAround = cpred . cpred

findTurn :: Direction -> Direction -> Turn
findTurn d1 d2 = head $ filter (\t -> orient t d1 == d2) range -- becoz `range :: [Turn]` gives [TNone, TLeft, TRight, TAround]

class (Enum a, Bounded a) => BoundedEnum a where
  range :: [a]
  range = enumFrom minBound

-- StandaloneDeriving gives us the option to allow derivation when we don't
-- have access to the datatypes directly. Very nifty trick indeed, of course
-- depending on the property of the datatypes, the derivation might need
-- modifications which the developer has no access to ... that's quite another
-- problem.
deriving instance BoundedEnum Direction
deriving instance Eq Turn
deriving instance Ord Turn

test :: Bool
test = sort (nub [findTurn d1 d2 | d1 <- range, d2 <- range]) == range

