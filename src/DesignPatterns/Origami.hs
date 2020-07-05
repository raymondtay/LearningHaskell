
module Origami where

-- Origami is the japanese art of folding and unfolding 
--
import Data.Bifunctor

--
-- Origami Programming refers to a style of generic programming that focuses on leveraging core patterns
-- of recursion: map, fold and unfold.
-- Keywords you might wish to think of : recursion schemes!
--


data List' a = Nil' | Cons' a (List' a ) deriving (Show)
data Tree  a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)

-- 's' represents the shape,
-- 'a' refers to an instance of the type.
-- The 'Fix' datatype is named after the fixed point of a function.
data Fix s a = FixT { getFix :: s a (Fix s a) }
-- The fixed point of a function is defined by
-- f (fix f) = fix f

-- Let's rewrite the List' and Tree in terms of Fix. Conceptually, you
-- replace the recursive parts with a new symbol 'r'.
-- E.g. List' a == r, Tree' a == r
--
data List_ a r = Nil_ | Cons_ a r deriving (Show)
data Tree_ a r = Leaf_ a | Node_ a r r deriving (Show)

type ListF a = Fix List_ a
type TreeF a = Fix Tree_ a

aListF :: ListF Integer
aListF = FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))

aTreeF :: TreeF Integer
aTreeF = FixT (Node_ 1 (FixT (Leaf_ 2)) (FixT (Leaf_ 3)))

-- Next thing to do after this is to figure out how to compute this expression
-- ?
mapL :: (t -> a) -> Fix List_ t -> Fix List_ a
mapL f listF =
  case list_ of
    (Cons_ x r) -> FixT (Cons_ (f x) (mapL f r))
    Nil_        -> FixT Nil_
    where list_ = getFix listF
-- As the previous was about dealing with lists, this function is about dealing
-- with trees. Btw, it is no concidence that it looks familiar.
mapT :: (t -> a) -> Fix Tree_ t -> Fix Tree_ a
mapT f treeF =
  case tree_ of
    (Node_ x l r) -> FixT (Node_ (f x) (mapT f l) (mapT f r))
    (Leaf_ x)     -> FixT (Leaf_ (f x))
    where tree_ = getFix treeF

showListF :: (Show a) => ListF a -> String
showListF (FixT (Cons_ x r)) = (show x) ++ ", " ++ (showListF r)
showListF (FixT Nil_) = "Nil_"

showTreeF :: (Show a) => TreeF a -> String
showTreeF (FixT (Node_ x l r)) = (show x) ++ ", " ++ (showTreeF l) ++ ", " ++ (showTreeF r)
showTreeF (FixT (Leaf_ x)) = "Leaf_ " ++ (show x)


-- Now, we are ready to attempt to write a more generic `map` with the purpose
-- that we can make another step to eliminating boilerplate code. First, we
-- need to create Bifunctor instances of the data types we wish to support.
instance Bifunctor List_ where
  bimap f g Nil_        = Nil_
  bimap f g (Cons_ a r) = Cons_ (f a) (g r)

instance Bifunctor Tree_ where
  bimap f g (Leaf_     a) = Leaf_ (f a)
  bimap f g (Node_ a l r) = Node_ (f a) (g l) (g r)

-- Let's break down how this is done. First, we know that our values is mangled
-- up with `Fix` and we know for a fact that we need to get it out (which
-- explains why "getFix" is the first thing we do). That is, `getFix shape`
-- where `shape :: s a (Fix s a)`. Next thing we do is pass this to `bimap` and
-- it is defined that `f = id` and `g = genericFold f` and continues applying
-- `g` till it reveals the structure `Tree_ a r` or `List_ a r` and finally it
-- reveals its true form in the final application of this shape to `f`
genericFold :: Bifunctor s => ( s a b -> b ) -> Fix s a -> b
genericFold f = f . bimap id (genericFold f) . getFix

-- Now that we have a way of walking a data structure, the next thing to do is
-- to familiarise with it by exploring the various uses.
--
mapL' :: (t -> a) -> List_ t (Fix List_ a) -> Fix List_ a
mapL' f (Cons_ x xs) = FixT (Cons_ (f x) xs)
mapL' f Nil_         = FixT Nil_

addL :: Num a => List_ a a -> a
addL (Cons_ x r) = x + r
addL Nil_        = 0

addT :: Num a => Tree_ a a -> a
addT (Node_ x l r) = x + l + r
addT (Leaf_ x)   = x

f = genericFold addL $ genericFold (mapL' (+1)) aListF
g = genericFold addL aListF
h = genericFold addT aTreeF


-- Next, i want to explore the unfolding part of the puzzle and the general
-- structure of evaluation is i want to evaluate a function (i.e. stopF) to decide whether
-- to continue or not, if this function says "stop" then it's over else we push
-- on by passing the "next" value etc
--
unfoldL :: (t -> Bool) -> (t -> t) -> t -> [t]
unfoldL stopF nextF value =
  if stopF value then
                 []
                 else value : (unfoldL stopF nextF (nextF value))

-- here is an example of what it looks like
j = unfoldL (< (-10)) (\x -> x - 1) 10

genericUnfoldL :: Bifunctor s => (b -> s a b) -> b -> Fix s a
genericUnfoldL f = FixT . bimap id (genericUnfoldL f) . f

toList :: (Eq r, Num r) => r -> List_ r r
toList 0 = Nil_ 
toList n = (Cons_ n (n - 1))

-- toList' is a rewritten form of the function "j" wrote earlier,above.
toList' :: (Eq r, Num r) => (r -> Bool) -> r -> List_ r r
toList' f n = if f n then Nil_ else (Cons_ n (n - 1))

main :: IO ()
main = do
  putStrLn . showListF $ mapL (+1) aListF
  putStrLn . showListF $ mapL (*2) aListF
  putStrLn . showTreeF $ mapT (*2) aTreeF
  putStrLn . show $ f -- map + add list
  putStrLn . show $ g -- add list
  putStrLn . show $ h -- add tree
  putStrLn . showListF $ genericUnfoldL toList 10
  putStrLn . showListF $ genericUnfoldL (toList' (< (-10))) 10

