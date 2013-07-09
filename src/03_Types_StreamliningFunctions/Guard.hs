fromMaybe defval wrapped = 
    case wrapped of 
        Nothing -> defval
        Just value -> value

-- bad pattern
data Fruit = Apple | Orange deriving (Show)
-- equational apple = Apple
-- equational orange = Orange
betterFruit f = case f of
                    "apple" -> Apple
                    "orange" -> Orange

-- otherwise :: Bool which has value True in Haskell
lend3 amount balance
        | amount <= 0            = Nothing
        | amount > reserve * 0.5 = Nothing
        | otherwise              = Just newBalance
        where reserve = 100
              newBalance = balance - amount
-- we can almost always translate the conditional expression to a 
-- pattern matching i.e. 
-- myDrop n xs = if n <= 0 || null xs then xs else myDrop (n - 1 ) xs
nicerDrop n xs | n <= 0 = xs
nicerDrop _ []          = []
nicerDrop n (x:xs)      = nicerDrop (n - 1) xs


