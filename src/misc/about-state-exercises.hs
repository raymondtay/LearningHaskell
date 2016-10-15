module AboutStates where

newtype State s a = State { runState :: s -> (a , s) }

{-
 - Actually, we have seen several newtypes whose contents are a function
 - particularly with our Monoid newtypes (Sum, Product, etc)
 - Newtypes must have the same underlying representation as the type
 - they wrap, as the newtype wrapper disappears at compile time. So the
 - function contained in the newtype must be isomorphic to the type it wraps.
 - That is, there must be a way to go from the newtype to the thing it wraps and
 - back again w/o losing information. For example, the following demonstrates
 - an isomorphism.
 -}

type Iso a b = (a -> b, b -> a) 
newtype Sum a = Sum { getSum :: a }

