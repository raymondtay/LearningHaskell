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

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

--
-- if the incoming value i.e. v is equal to a then there isn't a need to
-- replace the value of 'a' with that of 'v'
-- otherwise we continue to push either to the left tree or right tree
-- depending on whether its bigger or smaller
-- What you would notice is that in the end of this process, the tree is
-- sorted.
--
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' v Leaf = Node Leaf v Leaf
insert' v (Node left x right) 
  | v == x = Node left x right
  | v <  x = Node (insert' v left) x right
  | v >  x = Node left v (insert' v right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
  Node (mapTree f left) (f a) (mapTree f right)

testTree' ::  BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = 
  if mapTree (+1) testTree' == mapExpected
      then print "yup okay!"
      else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder = undefined

postorder :: BinaryTree a -> [a]
postorder = undefined

-- 
-- Over here, i made use of the preorder function to overcome the problem of
-- having to associated type constraints over the types 'a' and 'b'; there
-- seems to be a pattern here.
--
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x Leaf = x
foldTree f x tree = foldr f x (preorder tree)

-- As-patterns
--
-- These sorts of things are a good way to pattern match on part of something
-- and still refer to the entire original value.
--

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t




