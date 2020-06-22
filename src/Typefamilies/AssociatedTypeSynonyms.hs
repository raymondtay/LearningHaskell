{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Given this abstract looking type definitions
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


class GenericFD d r | d -> r where -- using functional dependencies, i define the transformation functions
  from :: d -> r
  to   :: r -> d

instance GenericFD (List' a) (RList a) where -- here is how to declare how the transformation actually works by defining it.
  from Nil'           = L U
  from (Cons' h t)    = R (Combo h t)
  to (L U)            = Nil'
  to (R (Combo x xs)) = Cons' x xs

-- This section onwards is to illustrate how to rewrite what you saw up there
-- (↑) using Associated Type Synonyms 
--
class GenericA d where
  type Rep d :: * -- Rep is a type function
  fromA :: d -> (Rep d)
  toA   :: (Rep d) -> d

instance GenericA (List' a) where
  type Rep (List' a) = (RList a) -- Rep type params must match the class params
  
  fromA Nil'           = L U
  fromA (Cons' x xs)   = R (Combo x xs)
  toA (L U)            = Nil'
  toA (R (Combo x xs)) = (Cons' x xs)

main = do
  print $ fromA (Cons' 1 Nil')
  print $ from (Cons' "1" Nil')

--
-- Notes about associated types versus fundeps
--
-- associated types have some benefits that fundeps don't have:
-- * Associated types provide explicit type functions contrary to the
-- implicit relations of functional dependencies
-- * Type functions allow us to reduce the number of type parameters for a
-- typeclass
-- * Type functions are more idiomatically functional than relational-stype
-- functional dependencies
--

