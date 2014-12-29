
-- Haskell provides us another way to create a
-- new type, using the 'newtype' keyword
-- Although their names are similar, the type and newtype keywords
-- have different purposes. The type keyword gives us another way of referring
-- to a type, like a nickname for a friend. we and the compiler know that
-- [Char] and String names refer to the same type.
-- 
-- In constrast, the newtype keyword exists to hide the nature of a type.
-- The compiler treats UniqueID as a different type from Int. As a user of 
-- a UniqueID, we know only that we have a unique identifier: we cannot see
-- that it is implemented as an Int.

data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewInt = N Int
    deriving (Eq, Ord, Show)

newtype UniqueID = UniqueID Int
    deriving (Eq)

-- when we declare a new type, we must choose which of the underlying type's typeclass
-- instances we want to expose. Here we have elected to make NewInt provide Int's instances
-- for Eq, Ord and Show. As a result, we can compare and print values of type NewInt.


-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

{-

When we change the declaration to the following expression, it now means that we can conduct
comparisons and how Haskell does that is that it applies the "==" to each field declared within 
the "Person" structure <=> the types of all the fields also have to be part of the Eq typeclass.
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Eq)

The next expression is to allow us to literally print out what a data type looks like by deriving 
our data type from the "Show" typeclass. Following that, we can improve by reading in a string which
defines our "Person" data type by deriving from the "Read" typeclass.

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Eq, Show, Read)
*Main> let p11 = read "Person {firstName = \"patrick\", lastName = \"tay\", age = 39}" :: Person
*Main> show p11
"Person {firstName = \"patrick\", lastName = \"tay\", age = 39}"

-}

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Eq, Show, Read)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Show, Read, Ord, Bounded, Enum)
{-
The previous expression allows us to do the following:
*Main> succ Monday
Tuesday
*Main> succ Saturday
Sunday
but doesn't allow us to to do the following:
*Main> pred Monday
*** Exception: pred{Day}: tried to take `pred' of first tag in enumeration
*Main> succ Sunday
*** Exception: succ{Day}: tried to take `succ' of last tag in enumeration
so it does look like the Enumeration we've defined in "Day" is such that Monday thru Sunday is presented
as "monotically increasing" but does not cycle through....i wonder how to solve this?
-}

infixr 5 :-: -- personally, this consumes too much keystrokes...
data List a = Empty
    | a :-: (List a ) deriving (Show, Read, Eq, Ord)

infixr 5 -|
(-|) :: List a -> List a -> List a
Empty -| xs = xs
(x :-: xs) -| ys = x :-: (xs -| ys)


