import Data.Char (digitToInt) 

asInt :: String -> Int

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x in loop acc' xs

asInt xs = loop 0 xs

-- Use a fold (choosing the appropriate fold will make your code much simpler) to 
-- rewrite and improve upon the "asInt" funciton from the previous

asInt2 (x:xs) = foldl digitToInt xs

