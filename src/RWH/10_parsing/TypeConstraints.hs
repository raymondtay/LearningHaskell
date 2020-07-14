{-

Adding a constraint for a type definition is essentially never a good idea.
It has the effect of forcing you to add type constraints to every function that
will operate on values of that type.

The solution is to omit type constraints from type definitions, and instead place them on 
the functions that need them.
-}

-- Have to set `:set -XDatatypeContexts` for the following to work
{-
data (Ord a) => OrdStack a = Bottom
    | Item a (OrdStack a)
    deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
  | a < b = isIncreasing rest
  | otherwise = False
isIncreasing _ = True
 
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s
-}

data OrdStack a = Bottom
    | Item a (OrdStack a)
    deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool -- we cannot drop the type-constraint here else '<' would not work.
isIncreasing (Item a rest@(Item b _))
  | a < b = isIncreasing rest
  | otherwise = False
isIncreasing _ = True
 
push :: a -> OrdStack a -> OrdStack a
push a s = Item a s

