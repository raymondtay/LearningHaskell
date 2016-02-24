module IterateUnfoldr where

-- Write the function myIterate using direct recursion.
-- Compare the behavior with the built-in iterate to gauge 
-- correctness. Do not look at the source or any examples
-- of iterate so that you are forced to do this yourself :-)
--
-- In GHC.List, iterate :: (a->a) -> a -> [a]
--
myIterate :: (a -> a) -> a -> [a]
myIterate f x = (f x) : myIterate f (f x)


-- Write the function myUnfoldr using direct recursion. Compare with the
-- built-in unfoldr to check your implementation. Again, don't look at
-- implementations of unfoldr so that you figure it out yourself.
--
-- 
myUnfoldr :: (b -> Maybe(a, b)) -> b -> [a]
myUnfoldr f x =
  case (f x) of
    Nothing     -> []
    Just (a, b) -> [a] ++ myUnfoldr f b


