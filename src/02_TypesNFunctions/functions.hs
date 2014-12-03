-- see 01_getting_started where mydrop.hs
-- contains the only function worked on 'lastButOne'

third (a, b, c) = c

-- there's no limit on how deep within a value a pattern can look.
-- This definition looks both inside a tuple and inside a list within that tuple
complicated (True, a , x:xs, 5) = (a, xs)

-- a failure to find a match given the pattern(s), a runtime exception will
-- be given.

type CustomerID = Int -- failure to define the type aliases before the decl would render it a load-file error.
type Address = [String]

data Customer = Customer {
    customerId :: CustomerID,
    customerName :: String,
    customerAddress :: Address
} deriving (Show)


data MList a = MCons a (MList a) | Nil deriving (Show)

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show) -- a binary tree, literally speaking. how cool is that ?!


