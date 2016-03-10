
import Data.List (elemIndex) 
import Data.Maybe 

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])

y :: Maybe Integer
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer
z = lookup 2 $ zip [1..3] [4..6]

tupled :: Maybe (Integer, Integer)
tupled = 
  case (y, z) of
    (Nothing, Nothing) -> Nothing
    (Nothing, _)  -> Nothing
    (_, Nothing)  -> Nothing
    (Just x, Just y) -> Just $ (,) x y


-- tupled = (, ) <*> y z
--

x :: Maybe Int
x = elemIndex 3 [1..5]

