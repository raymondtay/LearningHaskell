--
-- In parallel haskell code, the clutter that would arise from communication
-- code in a traditional language is replaced with the clutter of par and pseq
-- annotations.
--

import Control.Parallel

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) = let r = f x
                       in r `par` r : parallelMap f xs
parallelMap _ _ = []

-- Here's how a function forces every element of a list to be evaluated to
-- WHNF.
forceList :: [a] -> ()
forceList (x:xs) = x `pseq` forceList xs
forceList _ = ()

-- Here's how we can use it
stricterMap :: (a -> b) -> [a] -> [b]
stricterMap f xs = forceList xs `seq` map f xs

evalWHNF :: (Num a, Num b, Eq a, Eq b) => (a -> b) -> a -> b
evalWHNF f 1 = (f 1) `seq` (f 2)

