
-- i cheated a little by copying the type signature 
-- after loading the file and using (:t lend).
lend :: (Ord a, Num a) => a -> a -> Maybe a
lend amount balance = let reserve = 100
                          newbalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newbalance

-- In Haskell, a name in a let blopck is bound to an expression
-- and not to a value. As haskell's lazy, the expression associated
-- with a name won't actually be evaluated until it's needed. 
-- When we define a variable in a let block, we refer to it as a 
-- let-bound variable. This simply means what it says: We have bound
-- the variable in a let-block.

-- the following function is almost the same as the first
-- but its done using the "where clause". The definitions
-- in a where-clause apply to the code that precedes it. 
lend2 amount balance = if amount < reserve * 0.5 
                       then Just newbalance
                       else Nothing 
                    where reserve = 100
                          newbalance = balance - amount

-- the local function `plural x` defined within the where-clause
-- allows the enclosing variables to be used and manipulated upon.
makeitplural :: String -> [Int] -> [String]
makeitplural word counts = map plural counts
        where plural 0 = "no " ++ word ++ "s"
              plural 1 = "one " ++ word 
              plural n = show n ++ " " ++ word ++ "s"

niceDrop :: (Ord a, Num a) => a -> [t] -> [t]
niceDrop n xs | n <= 0 = xs
niceDrop _ [] = []
niceDrop n (_:xs) = niceDrop (n -1 ) xs
