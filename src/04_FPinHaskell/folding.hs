-- myfoldl :: (t -> t1 -> t) -> t -> [t1] -> t
myfoldl step zero (x:xs) = myfoldl step (step zero x) xs
myfoldl _    zero []     = zero

-- myfoldr :: (t -> t1 -> t1) -> t -> [t1] -> t
-- The class of functions that we can express using foldr is called
-- primitive recursive. A surprisingly large number of list manipulation 
-- functions are primitive recursive. 'map' can be expressed in terms of 'foldr'
-- and even 'foldl' can be expressed as 'foldr'
myfoldr step zero (x:xs) = step x (myfoldr step zero xs)
myfoldr _    zero []     = zero

myMap f xs = myfoldr step [] xs
    where step a as = f a : as

myfoldlviafoldr f z xs = myfoldr step id xs z
    where step x g a = g (f a x)

identity xs = foldr (:) [] xs

-- From the 'A tutorial on the universality and expressiveness of fold'
myfold f v [] = v
myfold f v (x:xs) = f x (myfold f v xs)

mysum :: [Int] -> Int
mysum = myfold (+) 0

myand :: [Bool] -> Bool
myand = myfold (&&) True

product :: [Int] -> Int
product = myfold (*) 1

-- Haskell provides a handy notational shoartcut to let us write partially applied function
-- in infix style. If we enclose an operator in parentheses, we can supply its left or right
-- argument inside the parentheses to get a partially applied function. This kind of partial
-- application is called a section.

-- The pattern xs@(_:xs') is called an as-pattern and it means "bind th variable xs to the value
-- that matches the right side of the @ symbol." Comment out the line to see how the as-pattern
-- would work.
-- There's an additional benefit to using as-patterns, i know its starting to sound like a pun, 
-- and that is we are saving cycles and ram as allocation is not needed when employing as-patterns
-- but when applied to regular pattern matching you would notice that an additional node is allocated
-- during runtime. This runtime allocation may be cheap but it's not free.
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
--suffixes (x:xs) = (x:xs) : suffixes xs
suffixes _ = []

-- the equivalent of the above but without no-as pattern 
noaspattern :: [a] -> [[a]]
noaspattern (x:xs) = (x:xs) : noaspattern xs
noaspattern _     = []

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x) 

