module UDFTypeclasses where


-- automatic derivation
data Color = Red | Green | Blue
  deriving (Show, Read, Eq, Ord)

colorEq :: Color -> Color -> Bool
colorEq Red Red     = True
colorEq Green Green = True
colorEq Blue Blue   = True
colorEq _ _         = False

stringEq :: [Char] -> [Char] -> Bool
stringEq [] []         = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _ _           = False

-- this says that we are declaring a typeclass named BasicEq, and we will
-- refere to instance types with the letter a. An instance type of this
-- typeclass is any type that implements the functions defined in the
-- typeclass. This typeclass defines one function.
class BasicEq a where
  isEqual :: a -> a -> Bool
  isEqual x y = not (isNotEqual x y)
  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)

instance BasicEq Bool where
  isEqual True True   =True
  isEqual False False = False
  isEqual _ _         = False

instance BasicEq Color where
  isEqual Red Red     = True
  isEqual Green Green = True
  isEqual Blue Blue   = True
  isEqual _ _         = False


