{-# LANGUAGE InstanceSigs, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

class Collects elementtype containertype | containertype -> elementtype where
  empty :: containertype
  insert :: elementtype -> containertype -> containertype
  member :: elementtype -> containertype -> Bool

instance Collects elementtype [elementtype] where
  empty = []
  insert = undefined
  member = undefined

instance Collects elementtype (elementtype -> Bool) where
  empty = undefined
  insert = undefined
  member = undefined

class C a b where

class D a b | a -> b

class E a b | a -> b, b -> a

instance D Bool Int
--instance D Bool Char -- this is a problem as the compiler complains of it.
instance D Int Bool -- this otoh, is not a problem.

instance E Bool Int
--instance E Bool Char -- this is like previous examples and a compiler error.
instance E Int Bool -- i would have thought that the 1:1 relationship i.e. a -> b, b -> a meant that this is going to be an issue but it isnt'

class Add a b c | a b -> c where
  plus :: a -> b -> c

class Mul a b c | a b -> c where
  mul :: a -> b -> c

instance Mul Int Int Int where
  mul = \a -> \b -> (*) a b

instance Mul Int Float Float where
  mul = \a -> \b -> (*) (fromIntegral a) b

instance Mul Float Int Float where
  mul = \a -> \b -> (*) (fromIntegral b) a

instance Mul Float Float Float where
  mul = (*)


-- A finite map is an indexed collection of elements that provides operations
-- to lookup the value associated with a particular index, or to add a new
-- binding. This can be described by a class:
--
class FiniteMap i e fm | fm -> i e where
  emptyFM :: fm
  lookup :: i -> fm -> Maybe e
  extend :: i -> e -> fm -> fm

-- here, fm is the finitemap type which uniquely determines both the index type
-- i and the element type e. Association lists, functions and arrays all fit
-- naturally into this framework.
--
instance FiniteMap i e [(i, e)] where
  emptyFM :: [(i, e)]
  emptyFM = []
  lookup :: i -> [(i, e)] -> Maybe e
  lookup = undefined
  extend :: i -> e -> [(i, e)] -> [(i, e)]
  extend = undefined



