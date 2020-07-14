{-
    A typeclass is a sort of interface that defines some 
    behavior. A type can be made an instance of a typeclass if it
    supports that behavior. E.g. the 'Int' type is an instance of the 'Eq' 
    typeclass because the 'Eq' typeclass defines behavior for stuff that 
    can be equated. And because integers can be equated, 'Int' is a part of 
    the 'Eq' typeclass.
    The real usefulness comes with the functions that act as the 
    interface for 'Eq' namely == and /=. If a type is a part of the 'Eq'
    typeclass , we can use the == functions with the values of that type.
    That's why expressions like '4 == 4' and 'foo /= bar' typecheck.
-}
module MyVector where

data Vector a = Vector a a a deriving (Show)

plus (Vector i j k) (Vector a b c) = Vector (a+i) (b+j) (k+c)

mult (Vector i j k) (Vector a b c) = Vector (i*a) (j*b) (k*c)

data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x  = Node x EmptyT EmptyT

insertT :: (Ord a) => a -> Tree a -> Tree a
insertT n EmptyT = singleton n
insertT n (Node a left right) 
    | n == a = Node n left right
    | n <  a = Node a (insertT n left) right
    | n >  a = Node a left (insertT n right)

elemT :: (Ord a) => a -> Tree a -> Bool
elemT n EmptyT = False
elemT n (Node a left right) 
    | n == a = True
    | n <  a = elemT n left
    | n >  a = elemT n right

data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where
    show Red    = "Red Light"
    show Green  = "Green Light"
    show Yellow = "Yellow Light"

instance Eq TrafficLight where
    Red == Red       = True
    Green == Green   = True
    Yellow == Yellow = True
    _ == _           = False
