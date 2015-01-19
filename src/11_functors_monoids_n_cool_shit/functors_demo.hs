module FunctorDemo where

main = do 
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards!"

{-
another equivalent way to write the previous express at line 4 is
the following :
main = do line <- fmap reverse getLine
    ... blah blah blah 
    ... blah blah

 and this works because the type signatures match
> :t getLine 
getLine :: IO String 
> :t reverse 
reverse :: [a] -> [a]
> :t fmap 
fmap :: (a -> b) -> f a -> f b
and expression of the entire expression is IO [Char]
since [Char] is synonymous to `String`
-}

