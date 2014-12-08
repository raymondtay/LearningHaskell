{-# LANGUAGE FlexibleInstances #-}

class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

-- We have two instances of the typeclass Borked for pairs:
-- one for a pair of ints and another for a pair of anything else that's Borked
-- Suppose we want to bork a pair of Int values. To do so, the compiler
-- must choose an instance to use. Because these instances are right next 
-- to each other, it may seem that it could simply choose the more specific instance.

-- However, GHC is conservative by default and insists that there must be only
-- one possible instance that it can use. It will thus report an error if we try
-- to use bork but a workaround is to use the pragma 'FlexibleInstances' like 
-- on the first line in this file.

instance Borked (Int, Int) where
    bork (a,b) = bork a ++ ", " ++ bork b

instance (Borked a,Borked b) => Borked (a,b) where
    bork (a,b ) = ">> " ++ bork a ++ " " ++ bork b ++ " <<"

