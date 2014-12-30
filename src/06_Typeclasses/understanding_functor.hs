{-

The functor typeclass is basically something that can be mapped over.
We begin by looking at what it means to be a functor which has 
1 simple function 'fmap' where 'f' is not a concrete type 
since an expression like 'f a' is a concrete type but 'f' by 
itself is consider a type constructor, rightfully so.

-}
module MyFunctor where

class F f where -- renamed to 'F' instead of 'Functor' since Prelude includes a Functor, by default. 
    fmap :: (a -> b) -> f a -> f b

instance F [] where
    fmap = map

instance F Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing  = Nothing
{-

When you load this file initially, you'll noticed that fmap = map
is allowed. Looking at the type signature of 'map'

 map :: (a -> b) -> [a] -> [b]

and when you compare that to the defn of 'fmap' you'll discover
that the list looks like a functor which indeed it is because []
is a value that defines the empty list whilst [a] defines a list
of 'a' and hence its a concrete type and if you're wondering why, 
hit the following into the command line and you'll know why:

*MyFunctor> :i []
data [] a = [] | a : [a]    -- Defined in ‘GHC.Types’
instance F [] -- Defined at understanding_functor.hs:15:10
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Monad [] -- Defined in ‘GHC.Base’
instance Functor [] -- Defined in ‘GHC.Base’
instance Ord a => Ord [a] -- Defined in ‘GHC.Classes’
instance Read a => Read [a] -- Defined in ‘GHC.Read’
instance Show a => Show [a] -- Defined in ‘GHC.Show’

-}

