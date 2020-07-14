import Data.Char(toUpper)

-- the following expression? one way to understand this 
-- is to check the type of the smaller sub-expressions
-- interact :: (String -> String) -> IO ()
-- (map toUpper) :: [Char] -> [Char] <=> String -> String
-- => interact (map toUpper) :: IO ()

main = interact (map toUpper)

another_main = interact (map toUpper . (++) "Your data in uppercase is :\n\n")

another_main_2 = interact ((++) "Your data, in uppercase, is :\n\n" . map toUpper)
