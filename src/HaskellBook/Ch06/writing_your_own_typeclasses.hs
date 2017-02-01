module Chapter06 where

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

--
-- let x = Trivial'; y = Trivial'
-- x == y
--

{--

1. The keyword `instance` here begins a declaration of a typeclass instance. Typeclass isntances are how you tell HAskell how equality, strinfgification, orderability, enumeration or other typeclasses should work for a particular data type. w/o this insance, we cannot test the values for equality even though the answer will never vary in the case of this particular datatype.

2. The first name to follow the `instance` is the typeclass the instance is providing. Here that is `Eq`.

3. The type the instance is being provided for. In this case, we are implementing the Eq typeclass for the Trivial datatype.

4. The keyword where terminates the initial declaration and beginning of the instance. What follows are the actual methods being implemented.

5. The data constructor Trivial' is the first argument to the == function we are providing. Here we are defining == using infix notataion so the first argument is to the left.

6. The infix function ==, this is what we are defininig in this declaration.

7. The second argument, whichis the value Trivial'. Since == is infix here, the second arugment is to the right of ==.

8. The result of Trivial' == Trivial' is `True`.

9. We could have written the defintion of (==) using prefix notation instead of infix by wrapping the operator in parentheses. Note that this is just being shown as an alternative; you cannot have two typeclasses of the same type.  
--}

data ThisAnInteger = TisAn Integer
instance Eq ThisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y 

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = (a == c) && (b == d)

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False
 
-- Type-constraint on `a` which must be `Eq`
data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = (a == c) && (b == d)

-- Type-constraint on `a` and `b` which must be `Eq`
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = (a == c) && (b == d)

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  -- redundant (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThisOne x) (ThatOne y) = x == y
  (==) (ThatOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) (ThisOne x) (ThisOne y) = x == y

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) (Hello _) (Goodbye _) = False
  (==) (Goodbye _) (Hello _) = False

-- A few things to keep in mind about writing Ord instances: First, it is wwise
-- to ensure that your Ord instances agree with your Eq instances. whether the
-- Eq instances are derived or manually written. If x == y, then compare x y
-- would return EQ. Also you want your Ord instances to define a sensible total
-- order. You ensure this in part by covering all cases and not writing partial
-- instances as we noted above with Eq In general, your Od instance should be
-- written such that when compare x y returns LT then compare y x would return
-- GT.
--


f :: Int -> Int
f blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show (f 4 :: Int))


