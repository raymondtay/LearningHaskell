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
-- Separating Algorithm from Evaluation
--
-- An important skill to master
--
-- In parallel haskell code, the clutter that would arise from communication
-- code in a traditional language is replaced with the clutter of `par` and
-- `pseq` annotations. As an example, this function operates similarly to
-- `map`, but evaluates each element to WHNF in parallel as it goes:
--
-- parallelMap :: (a -> b) -> [a] -> [b]
-- parallelMap f (x:xs) = let r = f x in r `par` r : parallelMap f xs
-- parallelMap _ _ = []
--
-- The type 'b' might be a list or some other type for which evaluation to
-- WHNF doesn't do a useful amount of work. We would prefer not to ahve to
-- write a special parallelMap for lists and every other type that needs
-- special handling.
--
-- To address this problem we will begin by considering a simpler problem : how
-- to force a value to be evaluated. Here is a function that forces every
-- element of a list to be evaluated to WHNF.
--
-- forceList :: [a] -> ()
-- forceList (x:xs) = x `pseq` forceList xs
-- forceList _ = ()
--
-- Our function performs no computations on the list. In fact, from examining
-- its type signature, we can tell that it cannot perform any computation,
-- since its knows nothing about the elements of the list. Its only purpose is
-- to ensure that the spine of the list is evaluated to head normal form. The
-- only it makes any sense to apply this function is in the first argument of
-- seq or par, as follows:
-- 
-- stricterMap :: (a -> b) -> [a] -> [b]
-- stricterMap f xs = forceList xs `seq` map f xs
--
-- This still leaves us with the lements of the list evaluated only to WHNF. We
-- address this by adding a function as parmeter that can force an element to
-- be evaluated more deeply:
--
-- forceListAndElts :: (a -> ()) -> [a] -> ()
-- forceListAndElts forceElt (x:xs) =
--   forceElt x `seq` forceListAndElts forceElt xs
-- forceListAndElts  _ _ = ()
--
-- The Control.Parallel.Strategies module generalizes this idea by introducing
-- the idea of an evaluation strategy:
--
--
-- type Done = ()
--
-- type Strategy a = a -> Done
--
-- An evaluation strategy performs no computation; it simply ensures that a
-- value is evaluated to some extent. The simplest strategy is named "r0", and
-- does nothing at all:
--
-- 
-- r0 :: Strategy a
-- r0 _ = ()
--
-- Next is "rwhnf" , which evaluates a value to WHNF :
--
-- rwhnf :: Strategy a
-- rwhnf x = x `seq` ()
--
-- To evaluate a value to normal form, the module provides a typeclass with a
-- method named "rnf":
--
-- class NFData a where
--   rnf :: Strategy a
--   rnf = rwhnf
--
-- For the basic types, such as Int, weak head normal form and normal form are
-- the same thing, which is why the NFData typeclass rwhnf as the default
-- implementation of rnf. For many common types, the
-- Control.Parallel.Strategies module provides instances of NFData (like the
-- following)...
--
-- instance NFData Char
-- instance NFData Int
--
-- instance NFData a => NFData (Maybe a) where
--   rnf Nothing = ()
--   rnf (Just a) = rnf a
--
--   {- and so on ... -}
--
-- From these examples, it should be clear how you might write an NFData
-- instance for a type of your own. Your implementation of `rnf` must handle
-- every constructor and apply `rnf` to every field of a constructor.
--
--
