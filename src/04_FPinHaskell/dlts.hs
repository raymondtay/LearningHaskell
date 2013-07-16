import Data.List (isPrefixOf)

-- We can create new functions at any time by writing chains of composed functions, 
-- stitched together with (.) so long as the result type of the function on the right
-- of each (.) matches the type of parameter that the function on the left can accept

-- We treat an entire file as a string, split it up with 'lines' and then apply 'foldr step []'
-- to the resulting list of lines. The 'step' helper function operates on a single line:

dlts :: String -> [String]

dlts = foldr step [] . lines
   where step l ds
            | "#define DLT_" `isPrefixOf` l = secondWord l : ds
            | otherwise = ds
         secondWord = head . tail . words
-- Question: Which expression does (.) bind to the left of "dlts = foldr step []" ?
-- Answer  : Generally, it binds to the entire 'foldr step []' to form a general expression of the 
--           form: (foldr step [] .)
--           Depending on the type of the 'step' function and the type of the function bound on the RHS
--           of the expression, it can yield very different results
--           The two examples show what happens when you change the type of the 'step' function
--           Notice that we only focus on the LHS of the expression 
--           *Main Data.List Data.Char> :t (foldr (++) [] . )
--           (foldr (++) [] . ) :: (a -> [[a1]]) -> a -> [a1]
--           *Main Data.List Data.Char> :t (foldr (+) [] . )
--           (foldr (+) [] . ) :: Num [a1] => (a -> [[a1]]) -> a -> [a1]

--           When we again focus on the RHS by adding a function "lines" to the end of these two examples
--           btw, the type of "lines" :: String -> [String] and the compiler evaluates a PASS/FAIL on the two
--           examples.
--           <interactive>:1:8:
--           No instance for (Num [Char]) arising from a use of `+'
--           Possible fix: add an instance declaration for (Num [Char])
--           In the first argument of `foldr', namely `(+)'
--           In the first argument of `(.)', namely `foldr (+) []'
--           In the expression: (foldr (+) [] . lines)
--           *Main Data.List Data.Char> :t (foldr (++) [] . lines)
--           (foldr (++) [] . lines) :: String -> [Char]
