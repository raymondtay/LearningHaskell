{-# LANGUAGE GADTs #-}

module SoP where


-- It is quite normal for Haskell programmers to represent Tree or Lists ADTs
-- in the following way; also writing functions to support this form of writing
-- is accepted.
--
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)

data List' a = Nil' | Cons' a (List' a) deriving (Show)

-- Its quite common to write functions like the ones below
sizeList Nil'         = 0
sizeList (Cons' x xs) = 1 + (sizeList xs)
-- count the number of nodes and leaves
sizeTree (Leaf _) = 1
sizeTree (Node _ l r) = 1 + (sizeTree l) + (sizeTree r)

-- Another way is to apply datatype generic technique. First, we have to start
-- from datatype-generic way.
--
-- The sum of products style of representation is another way to write the
-- above representations. There are a few conventions i had to understand
-- prior..
--
-- The commonality between the List and Tree ADT is that there is a terminal
-- nodes and the remaining are non-terminal constructs. Therefore, we need to
-- be able to express "Nil", "choice of either" and "combination of".
--
-- (a) constructors that consume no arguments (e.g. Nil') are represented by
-- "U".
-- (b) next, we encode the choice between multiple constructors in the style of
-- the "Either" type in the form taken by "Choice".
-- (c) finally, we encode type constructors with multiple arguments, we define
-- the "Combo" data type.
-- 
--

-- BEGIN of representation's definition
data U = U deriving (Show)

data Choice a b = L a | R b deriving (Show)

data Combo a b = Combo a b deriving (Show)

-- RList can be read as either U or its a combination of "a (List' a)" and it
-- does not recurse (this is an important point) which i will demonstrate.
type RList a = Choice U (Combo a (List' a))

type RTree a = Choice (Combo U a) -- encodes "Leaf"
                      (Combo a (Combo (Tree a) (Tree a))) -- encodes the value of type 'a' and both branches

-- END of representation's definition


-- Now, i can write functions to translate between type and representation
-- Let's start with the "List' a <-> RList a".
fromList :: List' a -> RList a
fromList Nil'         = L U
fromList (Cons' x xs) = R (Combo x xs)

toList :: RList a -> List' a
toList (L U)            = Nil'
toList (R (Combo x xs)) = Cons' x xs

fromTree :: Tree a -> RTree a
fromTree (Leaf a)      = L (Combo U a)
fromTree (Node a l r ) = R (Combo a (Combo l r))

toTree :: RTree a -> Tree a
toTree (L (Combo U a))           = Leaf a
toTree (R (Combo a (Combo l r))) = Node a l r

-- There's a fundamental problem with this approach : its very shallow
-- and the name given to this is : shallow type representation;
-- The best way to see this is to look at how some expressions would turn
-- out. If functions were written to handle the (List' a) and (Tree a) types
-- the "usual way", everything would have worked out very well.
--
-- Btw, don't be too quick to dismiss it yet ;)
--
-- > let t = Node 2 (Leaf 1) (Leaf 3)
-- > fromTree t
-- R (Combo 2 (Combo (Leaf 1) (Leaf 3)))
-- > let t = Node 3 (Node 2 (Leaf 1) (Leaf 0)) (Leaf 4)
-- > fromTree t
-- R (Combo 3 (Combo (Node 2 (Leaf 1) (Leaf 0)) (Leaf 4)))
--

data EP d r = EP { from :: (d -> r), to :: (r -> d) }

data TypeRepr t where
  RUnit   :: TypeRepr U
  RChoice :: TypeRepr a -> TypeRepr b -> TypeRepr (Choice a b)
  RCombo :: TypeRepr a -> TypeRepr b -> TypeRepr (Combo a b)
  RInt   :: TypeRepr Int
  RChar  :: TypeRepr Char
  RFloat :: TypeRepr Float
  RType  :: EP d r -> TypeRepr r -> TypeRepr d

-- With the "data generic" approach, "rList" is now a recursive function ;) which
-- overcomes the shortcomings of my previous approach.
rList :: TypeRepr a -> TypeRepr (List' a)
rList repr = RType (EP fromList toList) (RChoice RUnit (RCombo repr (rList repr)))

intList :: List' Int
intList = Cons' 1 (Cons' 2 (Cons' 3 Nil'))

-- Another function which leverages pattern-matching of the smart constructors
-- (e.g. RUnit, RChoice ...) to compute other functions
genericSize :: TypeRepr a -> a -> Int
genericSize RUnit  U  = 0
genericSize (RChoice trA trB) (L a) = genericSize trA a
genericSize (RChoice trA trB) (R b) = genericSize trB b
genericSize (RCombo  trA trB) (Combo a b) = (genericSize trA a) + (genericSize trB b)
genericSize RInt  _ = 1 
genericSize RFloat  _ = 1 
genericSize RChar  _ = 1 
genericSize (RType ep tr) t = genericSize tr (from ep t)

-- To see how "rList" would work, 
--
-- > let xs = R (Combo 1 (Cons' 2 (Cons' 3 Nil')))
-- >
-- > genericSize (RChoice RUnit (RCombo RInt (rList RInt))) xs
-- 3
-- 
-- > genericSize (RChoice RUnit (RCombo RInt (rList RInt))) (R (Combo 2 (Cons' 3 (Cons' 4 Nil'))))
-- 3
-- 
-- > genericSize (RCombo RInt (rList RInt)) ((Combo 2 (Cons' 3 (Cons' 4 Nil')))))
-- 3


--
-- Now, what happens when we add a new data type ? Do we need to re-write all
-- this wonderful code out there? Short answer is "No".
--

rTree :: TypeRepr a -> TypeRepr (Tree a)
rTree repr = RType (EP fromTree toTree)
                   (RChoice (RCombo RUnit repr) (RCombo repr (RCombo (rTree repr) (rTree repr))))

intTree :: Tree Int
intTree = (Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11)))
--
-- > genericSize (rTree RInt) (Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11)))
-- 5
--
-- > genericSize (RChoice RUnit (rTree RInt)) (R intTree)
-- 5<Paste>

main :: IO ()
main = do
  putStrLn . show $ "Size of list: " ++ (show intList) ++ " is " ++ (show $ genericSize (rList RInt) intList)
  putStrLn . show $ "Size of tree: " ++ (show intTree) ++ " is " ++ (show $ genericSize (rTree RInt) intTree)


