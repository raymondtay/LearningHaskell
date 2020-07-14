-- Though we defined our idea of a List, we noticed
-- that we have also defined it in terms of itself i.e. recursive type definition
-- And its quite common to do so in Haskell, as well as in Scala 
data List a = CONS a (List a)
              | Nil
                deriving (Show)

data Tree a = Node a (Tree a ) (Tree a)
              | Empty
                deriving (Show)

fromList(x:xs) = CONS x (fromList xs)
fromList []    = Nil
