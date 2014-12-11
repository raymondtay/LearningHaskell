
{- 
    Another common use of interact is filtering. Let's say that you want to write
    a program that reads a file and prints out every line that contains the character 'a'.

    typeOf(unlines) :: [String] -> String
    typeOf(lines) :: String -> [String]
    typeOf(elem) :: (Eq a) => a -> [a] -> Bool

-}

main = interact (unlines . filter (elem 'a') . lines)

