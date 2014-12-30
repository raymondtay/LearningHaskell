-- Start the ghci using this "ghci -v -XFlexibleInstances -XUndecidableInstances"
-- alternatively, whilst in the ghci enter the following :
-- :set -XFlexibleInstances
-- :set -XUndecidableInstances

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
instance Num a => Maybe a where
    Just a = a
    Nothing = 0
 
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

-- the above definition allows the following expressions to work:
-- *Main Map> yesno $ Just 333
-- True
-- *Main Map> yesno $ Just 333.3
-- True
-- *Main Map> yesno (Just 4)
-- True
-- 

