
-- 
-- a simple function on filter by mapping the predicate
-- function across the list
--
mfilter pred [] = []
mfilter pred (x:xs)
    | pred x    = x : mfilter pred xs
    | otherwise = mfilter pred xs

-- the let-block declaration demonstrates the
-- use of the predicate function in the function
-- we just developed.
let even x = if mod x 2 == 0 then True else False 
in mfilter even [1..100]
