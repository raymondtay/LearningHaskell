
module Programmers where

data OperatingSystem = 
  GnuPlusLinux
  | OpenBSD
  | Mac 
  | Windows deriving (Eq, Show)

data ProgrammingLanguage = 
  Haskell 
  | Agda
  | Idris
  | Java
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage } deriving (Eq, Show)

-- Turns out we can actually create a partial record, so as to speak
-- and the runtime only sends out a warning, at best.
{-
*Programmers> let partialAf = Programmer { os = Mac }

<interactive>:468:17: Warning:
    Fields of ‘Programmer’ not initialised: lang
    In the expression: Programmer {os = Mac}
    In an equation for ‘partialAf’: partialAf = Programmer {os = Mac}
*Programmers>

-}

-- A value of type P would be a product of two arguments,
-- one of type a and the other of type b.
-- Whether or not you choose to use infix data constructors, type 
-- constructors or typeclass names is down to aesthetic preference.
data P a b = a :*: b deriving (Eq, Show)

-- This tree has a value of type a at each node. Each node could be a temrinal
-- node, called a Leaf, or it could branch and have two subtrees. The 
-- subtrees are also of type BinaryTree a, so this type is recursive.
-- Each binary tree can store yet another binary tree, which allows for trees of arbitrary 
-- depth.
data BinaryTree a = 
  Leaf
  | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- data is immutable in Haskell.
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right) 


-- map function of a binary-tree
--
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)


-- Test and fixtures to be sure the map function 
-- `mapTree` works as expected
--
testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf) 

mapOkay = 
  if mapTree (+1) testTree' == mapExpected
  then print "Yup okay!" 
  else error "test failed"

