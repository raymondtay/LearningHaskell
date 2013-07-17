-- Avoiding space leaks with "seq"
-- We refer to an expression that is not evaluated lazily as strict, so foldl' is a strict left
-- fold. It bypasses HAskell's usual nonstrict evaluation through the use of a special 
-- function named "seq"

foldl' _ zero [] = zero
foldl' step zero (x:xs) = 
    let new = step zero x
    in new `seq` foldl' step new xs

-- seq operates as follows: when a seq expression is evaluated, it forces it argument to be 
-- evaluated, and then returns its second argument. It does not actually do anything with the 
-- first argument. seq exists solely as a way to force that value to be evaluated. 
-- Let's walk thru an example with the expression:
-- foldl' (+) 1 (2:[]) which is eq to foldl' (+) 1 [2] which expands to the following:
-- let new = 1 + 2 in new `seq` foldl' (+) new []
-- and the use of seq forcibly evaluates "new" to 3 and returns the second argument
-- and that means 
-- foldl' (+) 3 [] 
-- is returned and then "3" is returned as the final result
-- -----------------------------------------

-- To have an affect, the seq expression must be the first thing applying to the entire expression
-- and the following illustrates the correct and incorrect way.

-- incorrect: seq is hidden by the application of someFunc since someFunc will be evaluated
-- first, seq may occur too late
-- hiddenInside x y = someFunction(x `seq` y)

-- incorrect: a variation of the above mistake
-- hiddenByLet x y z = let a = x `seq` someFunc y in anotherFunc a z

-- correct: seq will be evaluated first, forcing evaluation of x
-- outside x y = x `seq` someFunc y

-- To strictly evaluate several values, chain applications of seq together:
someFunc value = \x -> x + value
-- chaining x y z = x `seq` y `seq` someFunc z
chaining x y z = seq x seq y someFunc z

-- in the previous expressions the seq will run first and the first expressions
-- on the equation will be invoked first.

-- Here, the intention is to evaluate "step zero x" strictly. Since the expression is
-- duplicated in the body of the function, strictly evaluating the first instance of it 
-- will have no effect on the second. The use of "let" from the defitinion of foldl' just shows
-- illustrates how to achieve this effect correctly.

-- NOTE: When evaluating an expression, seq stops as soon as it reaches a constructor. 
--       For simple types such as numbers, this means that it will evaluate them completely. 
--       Algebraic data types are a different story.

-- badExpression step zero (x:xs) = seq (step zero x) (badExpression step (step zero x) xs)

-- let pair a b = a `seq` b `seq` (a,b) in pair 3 3

strictPair (a,b) = a `seq` b `seq` (a,b)

strictList (x:xs) = x `seq` x : strictList xs
strictList []     = []

