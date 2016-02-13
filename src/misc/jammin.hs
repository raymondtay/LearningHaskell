
module Jammin where

data Fruit = 
  Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show)

-- before the record syntax, this is what it looked like ...
-- data JamJars = Jam Fruit Int deriving (Eq, Show)
--
data JamJars = Jam { typeOfFruit :: Fruit, numberOfFruit :: Int } deriving (Eq, Show)

{- here's an example of we can use the record syntax
 -
*Jammin> let j1 = Jam Peach 4
*Jammin> let j2 = Jam Apple 19
*Jammin> typeOfFruit j1
Peach
*Jammin> typeOfFruit j2
Apple
-}


