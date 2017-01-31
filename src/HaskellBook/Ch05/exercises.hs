module Chapter05 where

-- Provide an implementation given the type signature
-- 

i :: a -> a
i = id

c :: a -> b -> a
c = \x -> \y -> x

c'' :: b -> a -> b
c'' = \x -> \y -> x

r :: [a] -> [a]
r [] = []
r (x:xs) = x : r xs

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g 

a :: (a -> c) -> a -> a
a f = id
{--
 -- The key thing is break down the type signatures into its
 -- constituents. i.e. (a-> c) -> a -> a is actually 
 -- (a -> c) -> (a -> a) and we know the latter is `id`
 -- and we know for a fact that we do nothing with `c` after
 -- applying (a -> c) which is why its safe to ignore it.
 --}
a' :: (a -> b) -> a -> b
a' f = f
{--
 -- The key thing to understand this is to first recognize
 -- the 2 functions here. i.e. (a->b) -> a -> b actually 
 -- is (a -> b) -> (a -> b) since functions bind to the 
 -- extreme right. now it should be obvious why i'm simply
 -- return `f`.
 --}
