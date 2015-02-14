
> foldE :: (a -> c) -> (b -> c) -> Either a b -> c
> foldE f g (Left x) = f x
> foldE f g (Right x) = g x

> data Nat = Zero | Succ Nat deriving Show
> data NEList a = One a | Cons a (NEList a) deriving Show

> foldNat f Zero = f Zero : []
> foldNat f (Succ x) = f x : foldNat f (Succ (f x))

> foldNE f (One x) = f x : []
> foldNE f (Cons a xs) = f a : foldNE f xs

with the above defn of "foldNE" we can write the expression

-> foldNE (\e -> e + 1) (Cons 1 (Cons 2 (One 0)))
-> [2,3,1]

foldr1 differs from foldr in that the former ignores the first argument 
and applies to non-empty lists and by the definition of NEList, it cannot be 
empty since `One a` where `a` is some of unknown but existent type, it cannot be empty.

 
--------------------------------

Prove that 

foldl f e xs = foldr (flip f) e (reverse xs)

when xs is []
	LHS
	
	=> foldl f e []
	=> f e []
	=> [e]
	
	RHS
	
	=> foldr (flip f) e (reverse [])
	=> foldr (flip f) e []
	=> (flip f) e []
	=> f [] e 
	=> [e]

when xs is (x:y:xs)
	 LHS 
	 
	 => foldl f e (x:xs)
	 => f (e x) : foldl f e xs
	 ...
	 => f(..f( (f (e x)) x1) ... [])
	 
	 RHS 
	 
	 => foldr (flip f) e (reverse (x:y:xs))
	 => foldr (flip f) e (xs:y:x:[])
	 => ((flip f) ... ((flip f) y ((flip f) x ((flip f) e [])))...)

Right.
