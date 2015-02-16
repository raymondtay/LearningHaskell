module MyPrelude where

-- Start the ghci using this "ghci -v -XFlexibleInstances -XUndecidableInstances"
-- alternatively, whilst in the ghci enter the following :
-- :set -XFlexibleInstances
-- :set -XUndecidableInstances

import Prelude hiding((+), sum) -- hiding the default i.e. Prelude.show, Prelude.sum and Prelude.+

data AnotherFoo = AnotherFoo { x:: Integer, str :: String } deriving (Read, Show)

instance Eq AnotherFoo where
    (AnotherFoo i1 s1) == (AnotherFoo i2 s2) = (i1 == i2) && (s1 == s2)

class MShow a where
    mshow :: a -> String

instance MShow Bool where
    mshow True = "True"
    mshow False = "Flask"

instance MShow Int where
    mshow x = Prelude.show x

instance MShow a => MShow [a] where
    mshow xs = "[" ++ go True xs
        where 
            go _ [] = "]"
            go x (h:t) = if x then "" else ", " ++ mshow h ++ go False t

print :: MShow a => a -> IO ()
print x = putStrLn $ mshow x

class MNum a where
    fromInt :: Int -> a
    (+)     :: a -> a -> a

instance MNum Int where
    fromInt i = id i
    (+) a b = a + b

print_incr :: (MShow a, MNum a) => a -> IO ()
print_incr x = MyPrelude.print $ x + fromInt 1

print_incr_int :: Int -> IO ()
print_incr_int x = print_incr x

sum :: MNum a => [a] -> a -- hide Prelude.sum or else ...
sum xs = foldr (+) (fromInt 0) xs -- hide Prelude.+ or else GHC will frown at u

{- 
Here's an example how to use it:
*Main> let xs = show True
*Main> xs
"True"
*Main> let ys = show False
*Main> ys
"Flask"
-}

class (Eq a, Num a) => YesNo a where
    yesno :: a -> Bool

-- The YesNo defines 1 function "yesno" and that function takes 1 value of a type 
-- and tells us whether its true or not.
instance (Eq a, Num a) => YesNo a where 
    yesno 0 = False
    yesno _ = True

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

-- so, the previous declares what it means when "yesno" is applied to Integers.
-- *Main Map> yesno ( 0 :: Int)
-- False
-- *Main Map> yesno ( 9 :: Int)
-- True
-- But the following expression fails:
-- *Main Map> yesno 1

-- <interactive>:187:1:
--     No instance for (YesNo a0) arising from a use of `yesno'
--     The type variable `a0' is ambiguous
--     Possible fix: add a type signature that fixes these type variable(s)
--     Note: there are several potential instances:
--       instance YesNo [a] -- Defined at simple_typeclass.hs:13:10
--       instance YesNo Int -- Defined at simple_typeclass.hs:7:10
--     In the expression: yesno 1
--     In an equation for `it': it = yesno 1
-- 
-- <interactive>:187:7:
--     No instance for (Num a0) arising from the literal `1'
--     The type variable `a0' is ambiguous
--     Possible fix: add a type signature that fixes these type variable(s)
--     Note: there are several potential instances:
--       instance Num Double -- Defined in `GHC.Float'
--       instance Num Float -- Defined in `GHC.Float'
--       instance Integral a => Num (GHC.Real.Ratio a)
--         -- Defined in `GHC.Real'
--       ...plus 11 others
--     In the first argument of `yesno', namely `1'
--     In the expression: yesno 1
--     In an equation for `it': it = yesno 1
-- 
-- To fix the above situation, we define another instance but this time using "Num"
instance (Eq a, Num [a]) => YesNo [a] where
    yesno [] = False
    yesno _  = True

-- this one declares what it means when "yesno" is applied to a list of a's.
{-
instance Num a => Maybe a where
    Just a = a
    Nothing = 0
 
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

-}
-- the above definition allows the following expressions to work:
-- *Main Map> yesno $ Just 333
-- True
-- *Main Map> yesno $ Just 333.3
-- True
-- *Main Map> yesno (Just 4)
-- True
-- 

data Foo = F Int | G Char

instance Eq Foo where
    (F i) == (F j) = i == j
    (G a) == (G b) = a == b
    _ == _ = False

class Listable a where
    toList :: a -> [Int] 
instance Listable Int where
-- toList :: Int -> [Int]
    toList x = [x]
instance Listable Bool where
-- toList :: Bool -> [Int]
    toList True = [1]
    toList False = [0]
instance Listable [Int] where
-- toList :: [Int] -> [Int]
    toList = id

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

instance Listable (Tree Int) where
    toList Empty = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

instance (Listable a, Listable b) => Listable (a,b) where
    toList (a,b) = toList a ++ toList b

-- with the functor defined below, we can write something like
-- fmap (\e -> show e) (Node 4 (Node 45 Empty Empty) Empty)
-- Node "4" (Node "45" Empty Empty) Empty

instance Functor Tree  where
    fmap f Empty    = Empty
    fmap f (Node x left right) = (Node (f x) (fmap f left) (fmap f right))

class Tofu t where
    tofu :: j a -> t a j
{-
    'j' takes in 1 type 'a' => kind of 'j' is * -> *
    't' takes in 2 types, 'a' which is kind '*' and we know 'j' is (* -> *)
    therefore, 't''s kind is * -> (* -> *) 
    combinating both 'j a -> t a j' (* -> *) -> (* -> (* -> *)) -> *
                                    |<--j-->|   |<--- t ----->|
    but we can collapse this further by recognizing that 'j a' produces * on the LHS of ->
    and 't a j' produces '*' on the RHS
    which gives us the kind-expression: * -> (* -> *) -> *
-}

data Frank a b = Frank { field :: b a } deriving (Show)

instance Tofu Frank where
    tofu x = Frank x

data CMaybe a = 
    CNothing | 
    CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

{-
fmap id (CJust 0 "hey")
fmap \s -> s ++ s (CJust 0 "hey")
-}


