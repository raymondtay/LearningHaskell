
{-# LANGUAGE TypeFamilies #-}


-- Apparently, there is another form of "type families" that is top-level
-- type-families that are not associated with a type-class
-- Examples are lifted from [[AssociatedTypeSynonyms.hs]]
--

data List' a
  = Nil' | Cons' a (List' a) -- this is your "usual" list
  deriving (Show)

data U = U                 
  deriving (Show)

data Choice a b = L a | R b  -- this is your Either data type 
  deriving (Show)

data Combo a b = Combo a b   -- this is a combination
  deriving (Show)

type RList a = Choice U (Combo a (List' a)) -- this basically means that we want to construct a list that consists of "right" lists hence the name

type family RepF d
type instance RepF (List' a) = (RList a)

-- The type family RepF represents a type function, with each instance
-- declaring a value. Put in another way, a type family represents a set of
-- types , and each instance represents a set member.
--

class GenericF d where
  fromF :: d -> (RepF d)
  toF   :: (RepF d) -> d

instance GenericF (List' a) where
  fromF Nil' = L U 
  fromF (Cons' x xs) = R (Combo x xs)
  toF (L U) = Nil'
  toF (R (Combo x xs)) = (Cons' x xs)

main :: IO ()
main = do
  print $ fromF (Cons' 1 Nil')

-- With associated types, we need to align the type function paramerter swith
-- those of the type-class. Top-level type families don't have that restriction
-- and are therefore more general than associated types. The fundamental
-- difference between toplevel and associated type families is in the scope of
-- the type function.
--
-- Type families are to regular types what type class methods are to regular
-- functions. Instead of polymorphism over value, type families give us
-- polymorphism over datatypes.
--
-- As with type-classes, type families are "open" in the sense that we can add
-- new instances at any time.
--

