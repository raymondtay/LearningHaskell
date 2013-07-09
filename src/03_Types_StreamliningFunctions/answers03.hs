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
