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

-- Rewrite myIterate into betterIterate using myUnfoldr. A hint
-- we used unfoldr to produce the same results as iterate earlier.
--
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\e -> Just(e,f e)) x

data BinaryTree a = 
  Leaf 
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show, Ord)

-- write unfold for BinaryTree
--
unfold :: (a -> Maybe(a, b, c)) -> a -> BinaryTree b
unfold f x = 
  case (f x) of 
    Nothing -> Leaf
    Just (a,b,c) -> Node Leaf b Leaf

-- Make a tree builder
-- Using the unfold function you've just made for BinaryTree, write 
-- the following function

treeBuild :: Integer -> BinaryTree Integer
treeBuild n 
  | n <= 0    = Leaf 
  | otherwise = Node (treeBuild (n-1)) (n-1) (treeBuild (n-1))
  

