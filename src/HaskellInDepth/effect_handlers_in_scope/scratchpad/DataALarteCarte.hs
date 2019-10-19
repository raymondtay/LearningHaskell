{-#LANGUAGE TypeOperators,
            MultiParamTypeClasses,
            FlexibleInstances,
            FlexibleContexts,
            InstanceSigs,
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
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where -- identity is reflexive
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj         = Inl
  prj (Inl e) = Just e
  prj _       = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj          = Inr . inj
  prj (Inr ga) = prj ga
  prj _        = Nothing

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 ⊕ -- u+2295
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

data Mul x = Mul x x
instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

infixl 7 ⊗  -- its important to know that the multiplication operations precedes addition.
(⊗) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x ⊗ y = inject (Mul x y)

complexExample1 :: Expr (Val :+: Add :+: Mul)
complexExample1 = val 80 ⊗ val 4 ⊕ val 4 -- returns 324
complexExample2 :: Expr (Mul :+: Val :+: Add)
complexExample2 = val 80 ⊕ val 4 ⊗ val 4 ⊕ val 3 -- returns 99


{-
  Introduce a class, corresponding to the function we want to write. An obvious candidate for this 
  class is:

  class Render f where
    render :: f (Expr f) -> String

  The type of `render` is not general enough. To see this, consider the instance definition for `Add`.
  We would like to make recursive calls to the subtrees, which themselves might be values, for instance.
  The above type for render, however, requires that all subtrees of `Add` are themselves additions. 
  Clearly this is undesirable. A better choice is as follows:
-}
class Render f where
  render :: Render g => f (Expr g) -> String

-- This more general type allows us to make recursive calls to any
-- subexpressions of an addition, even if these subexpressions are not
-- additions themselves.
--
pretty :: Render f => Expr f -> String
pretty (In e) = render e

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add i j) = "(" ++ pretty i ++ " + " ++ pretty j ++ ")"

instance Render Mul where
  render (Mul i j) = "(" ++ pretty i ++ " * " ++ pretty j ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr x) = render x

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In e) = prj e

distribute :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distribute e = do
  Mul a b <- match e
  Add c d <- match b
  return (a ⊗ c ⊕ a ⊗ d)

-- if i were to write "val 80 ⊗ val 4 ⊕ val 6" then this example would not
-- generate the right instance but if an temporary was introduced then it would
-- generate the desired "expression". See below.
complexExample3 :: Expr (Val :+: Add :+: Mul)
complexExample3 = let x = val 4 ⊕ val 6
                  in val 80 ⊗ x

-- this should parse nicely
distributeExample1 =
  case (distribute complexExample3) of
      Nothing -> "This computation is not eligible for distribution."
      Just expr -> pretty expr

-- this should not parse
distributeExample2 =
  case (distribute complexExample2) of
      Nothing -> "This computation is not eligible for distribution."
      Just expr -> pretty expr
--
-- Monads for Free (Luth & Ghani, 2002) 
--

-- In general, the coproduct of 2 monads is fairly complicated (Luth & Ghani,
-- 2002) . No kidding !
-- 
data Term f a = Pure a | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure (fmap (fmap f) t)

instance Functor f => Applicative (Term f) where
  pure :: a -> Term f a
  pure = Pure

  (<*>) :: Term f (a -> b) -> Term f a -> Term f b
  (Pure f) <*> (Pure a) = Pure (f a)
  (Pure f) <*> (Impure t) = Impure (fmap (fmap f) t)
  (Impure fab) <*> a = Impure $ fmap (\e -> e <*> a) fab

instance Functor f => Monad (Term f) where
  return :: a -> Term f a
  return x = Pure x

  (>>=) :: Term f a -> (a -> Term f b) -> Term f b
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)


