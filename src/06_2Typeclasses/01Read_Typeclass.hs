
{-
  The Read typeclass is essentially the opposite of Show. It defines functions that will take a 
  String, parse it, and return data in any type that is a member of Read. The most useful 
  function in Read is `read`.
-}

-- 
-- the "parsing" is done implicitly when we the type of the desired value is
-- expressed as `Double` in the expression `(read input) :: Double`
-- 
-- what happens when you don't explicit state the type of the desired value???
-- You'll notice an exception being thrown "*** Exception: Prelude.read: no parse"
--
main = do
  putStrLn "Please enter a double: "
  input <- getLine
  let parsedData = (read input)::Double
  putStrLn("Twice of :[" ++ show parsedData ++ "] is [" ++ show (parsedData * 2) ++ "]")

