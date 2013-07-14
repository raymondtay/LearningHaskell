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

