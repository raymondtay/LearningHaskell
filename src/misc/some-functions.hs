
-- myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- myAny returns True if a -> Bool applied to any of the values
-- in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x then True else myAny f xs

-- myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

-- a reverse function
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- a mapping function
myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

-- filtering functionality
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) 
  | f x = [x] ++ myFilter f xs
  | otherwise = myFilter f xs

-- squish flattens nested lists
squish :: [[a]] -> [a]
squish [[]] = []
squish ([]:xs) = [] ++ squish xs
squish [(x:xs)] = [x] ++ squish [xs]
squish ((x:xs):ys) = [x] ++ squish [xs] ++ squish ys

-- squishMap maps a function over a list and concatenates the results
squishMap :: (a->[b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- squishAgain re-uses squishMap 
squishAgain :: [[a]] -> [a]
squishAgain [[]] = []
squishAgain ([]:xs) = [] ++ squishAgain xs
squishAgain [(x:xs)] = [x] ++ (squishMap (\t -> [t]) xs)
squishAgain ((x:xs):ys) = [x] ++ squishAgain [xs] ++ squishAgain ys

