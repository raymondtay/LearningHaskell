{-

Adding a constraint for a type definition is essentially never a good idea.
It has the effect of forcing you to add type constraints to every function that
will operate on values of that type.
-}

-- Have to set `:set -XDatatypeContexts` for the following to work

data (Ord a) => OrdStack a = Bottom
    | Item a (OrdStack a)
    deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
  | a < b = isIncreasing rest
  | otherwise = False
isIncreasing _ = True

