
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


