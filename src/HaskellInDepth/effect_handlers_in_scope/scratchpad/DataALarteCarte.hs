{-#LANGUAGE TypeOperators,
            MultiParamTypeClasses,
            FlexibleInstances,
            FlexibleContexts,
            OverlappingInstances #-}

-- NOTE: you will run into Overlapping instances problem. Use the corresponding
-- GHC extension to resolve it. See
-- https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#overlapping-instances
-- for details!!!
--
module DataALarteCarte where

-- I need to refresh myself on these wonderful ideas, no better way than
-- reinventing the wheel.
--
-- At the beginning, you can see how we can build functionalities that traverse
-- the data structure to perform all kinds of computations. It quickly becomes
-- a sticky wicket:
--
-- data Expr = Val Int | Add Expr Expr 
-- eval :: Expr -> Int
-- eval (Val x) = x
-- eval (Add x y) = (+) (eval x) (eval y)
-- 
-- render :: Expr -> String
-- render (Val x) = show x
-- render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

data Expr f = In (f (Expr f))

data Val e = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add

-- question: How do we combine both "IntExpr" and "AddExpr" ??
-- answer: we take the coproduct of their signatures. 
-- 
-- The coproduct of two signatures is straightforward to define in Haskell. It
-- is very similar to the Either data type; the only difference is that it does
-- not combine two base types, but two type constructors.
infixr 6 :+:
infixl 6 ⊕
data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In( Inr (Add (In (Inl (Val 118))) (In(Inl(Val 1219)))) )

addExample' :: Expr (Add :+: Val)
addExample' = In( Inl(Add (In (Inr (Val 118))) (In( Inr(Val 1219) ))))

-- The lesson from addExample and addExample' is that the "eval" does not
-- distinguish whether its Inl or Inr as long as the injection (in this case,
-- by hand) compiles.
--
instance Functor Val where
  fmap f (Val x) = Val x
instance Functor Add where
  fmap f (Add x y) = Add (f x) (f y)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)
-- Given the above 3 observations, we can define a generic fold over "Expr f"
--
foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x
instance Eval Add where
  evalAlgebra (Add a b) = a + b
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr x) = evalAlgebra x

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- val :: Int -> Expr Val
-- val x = In (Val x)

-- infixl 6 ⊕ -- u+2295

-- (⊕) :: Expr Add -> Expr Add -> Expr Add
-- x ⊕ y = In (Add x y)

--
-- addExample2 = val 1 ⊕ val 3 -- compilation error
--

-- The following is the key insight from this paper,
-- which is to create the mechanism to automate the "injections".
--
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where -- identity is reflexive
  inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

--infixl 6 ⊕ -- u+2295

(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

-- Its important to contrast the approach here versus the one in "addExample"
-- which is more straightforward and there is a problem with the Overlapping
-- instances problem in this approach
-- see https://www.reddit.com/r/haskellquestions/comments/3nc55l/data_types_a_la_carte/ 
-- for the discussion and potential solution.
--
addExample2 :: Expr (Add :+: Val)
addExample2 = val 30000 ⊕ val 1778 ⊕ val 4

