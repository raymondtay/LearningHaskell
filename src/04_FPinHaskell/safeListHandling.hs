-- Write your own 'safe' definitions of the standard partial list functions
-- but make sure they never fail. As a hint you might want to consider the 
-- following types:

import Data.List 

-- This function is a design to lift common designs 
-- into the a separate function 'liftSafe' and allow
-- the developer to compose higher-order functions with it.
liftSafe :: (a -> a) -> (a -> Bool) -> (a -> a)
liftSafe func test val = if test val then val else func val

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs)  = safeLast xs

-- safeInit :: [a] -> Maybe [a]
safeInit xs = liftSafe init null xs

