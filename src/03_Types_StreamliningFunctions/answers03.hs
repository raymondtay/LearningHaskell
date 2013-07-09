import Data.List
-- Write a function that computes the number of elements in the list. To test it
-- ensure that it gives the same answers as the standard length function
mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + (mylength xs)

-- write a function that computes the mean of a list
-- fromIntegral :: (Integral a , Num b) => a -> b
add a b = a + b
meanOfList [] = 0
meanOfList (x:xs) = let len = mylength xs in (foldl1 add xs)/(fromIntegral len)

makePalindrome [] = []
makePalindrome (x:xs) = (x:xs) ++ (reverse (x:xs))

isPalindrome []     = True
isPalindrome (x:xs) = x == (last xs) && isPalindrome (init xs)

-- Create a function that sorts a list of lists based on the length of each sublist.
-- You may want to look at 'sortBy' in the Data.List module
-- The solution here is to develop an association between the length of each sublist
-- to the actual list hence 'zip (map length a)' would do that
-- Next, we needed to sort this new association and we use Data.List's sortBy and 
-- afterwards, we would use the 'unzip' to remove the grouping for the length
-- which results in a 2-tuple which we extract the rhs of that tuple. Done
sortListOfLists :: Ord a => [[a]] -> [[a]]
sortListOfLists [] = []
sortListOfLists (l:ls) = snd (unzip (sortBy compare (zip (map length (l:ls)) (l:ls))))

-- Define a function that joins a list of lists together using a separator value
-- The separator should appear between elements of the list, but it should not follow
-- the last element.
myintersperse :: a -> [[a]] -> [a]
myintersperse _ [] = []
myintersperse c (x:[]) = x
myintersperse c (x:xs) = x ++ [c] ++ (myintersperse c xs)

data Tree a = Node a (Tree a ) (Tree a)
              | Empty
                deriving (Show)

height :: (Num a, Ord a) => Tree t -> a
height Empty = 0
height (Node x lhs rhs) = 1 + max (height lhs) (height rhs)
