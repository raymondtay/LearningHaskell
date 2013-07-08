-- error :: String -> a 
-- is the standard error function in Haskell and the
-- result type of a is so that we can call it anywhere 
-- and it will always have the right type. However, it does
-- not return a value like a normal function. 
-- Instead, it immediately aborts evaluation and prints the error message we give it.

mySecond :: [a] -> a

mySecond xs = if null (tail xs) 
              then error "list too short"
              else head (tail xs) 

-- we can use the Maybe type to represent the possibility of an error
-- If we want to indicate that an operation has failed, we can use the Nothing constructor
-- otherwise we wrap our value with the Just constructor.

safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
-- function's argument 'a' is never used in the body of the function
-- as it's being shadowed by the let-bound a, the argument can have any type at all
quux a = let a = "foo" in a ++ "eek!"

-- Local functions can be defined in the 'where' clause
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

