module Chapter11 where

import Data.Int

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

data BigSmall = Big Bool | Small Bool deriving (Eq,Show)

--
-- For reasons that will nbecome obvious, a cardinality of 2 makes it harder to
-- show the difference between sum and product cardinality.
-- The reason it is important to understand cardinality is that the cardinality
-- of a data type roughly equates to how difficult it is to reason about.
--
-- Records in Haskell are product types with additional syntax to provide
-- convenient accessors to fields within the record. 
--
data Person = MkPerson String Int deriving (Eq, Show)

data Person' = Person { name::String, age::Int }deriving (Eq, Show)

-- with the declaration of Person', we get `name` and `age` for free!
--
jm = Person "julie" 108
ca = Person "chris" 16

data Id a = MkId a deriving (Eq, Show) 

-- Because Id has an argument, we have to apply it to something before we can
-- construct a value of that type.
--
idInt :: Id Integer
idInt = MkId 10

-- As we have said throughout the book, one of the functional parts of
-- functional programming is that functions themselves are merely values. So we
-- can also do this:
--
idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

-- This is a little odd. The type Id takes an argument and the data constructor
-- MkId takes an argument of the corresponding polymorphic type. So, in order
-- to jhave a value of type Id Integer, we need to apply a -> Id a to an
-- Integer value. This binds the a type variable to Integer and applies away
-- the (-> ) in the type constructor, giving us Id Integer. We can also
-- construct a MkId value that is an identity function by binding the a to a
-- polymorphic function in both the type and the term level.
--


-- The first thing to notice is that you can construct values of products that
-- use record syntax in a manner identical to that of non-record products.
-- Records are just syntax to create field references. They don't do much heavy
-- lifting in Haskell but they are convenient.
--
--

-- Do take propagate bottoms throughout record types, and please do not do so. 
--
data Automobile = Null
                | Car { make :: String, model :: String, year :: Int } deriving (Eq, Show)

-- Terrible ↑ thing to do because `Null` should be replaced by `Maybe` instead.
-- How do we fix this? Well, first, whenever we have a product that uses record
-- accessors, keep it separate of any sum type that is wrapping it. To do this,
-- split out the product into an independent type with its own type constructor
-- instead of only as an inline data constructor product.
--





