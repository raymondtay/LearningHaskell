
import Data.List (elemIndex) 
import Data.Maybe 

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])

y :: Maybe Integer
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer
z = lookup 2 $ zip [1..3] [4..6]

tupled :: Maybe (Integer, Integer)
tupled = Just $ (,) (fromMaybe 1 y) (fromMaybe 1 z)

x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = Just $ max' (fromMaybe 1 x') (fromMaybe 1 y')

xs = [1..3]
ys = [4..6]

a :: Maybe Integer
a = lookup 3 $ zip xs ys

b :: Maybe Integer
b = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum $ (,) a b

