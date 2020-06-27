
{-# LANGUAGE GADTs #-}

import Control.Monad

-- Generalized Algebraic Data Types generlise ordinary algebraic data types by
-- allowing constructors to have richer return types.
-- Reading material:
-- * https://wiki.haskell.org/GADTs_for_dummies
-- * https://wiki.haskell.org/Generalised_algebraic_datatype
--

data Term a where
  Lit :: Int -> Term Int
  Succ :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a
  Pair :: Term a -> Term b -> Term (a, b)

-- Here is a interpreter of how these expressions can be evaluated
eval :: Term a -> a
eval (Lit i) = i
eval (Succ t) = 1 + eval t
eval (IsZero t) = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)

data Choice a b where
 L :: a -> Choice a b
 R :: b -> Choice a b

eval3 :: Choice a b -> Either a b
eval3 (L i) = Left i
eval3 (R j) = Right j

-- The key point about GADTs is that "pattern matching" causes type refinement.
-- For example, in the RHS of the equation
-- eval :: Term a -> 
-- eval (Lit i) = ...
-- the type a is refined to Int. The general principle is this : type
-- refinement is only carried out based on user-supplied type annotations.
--

eval2 :: Term a -> a -> a
eval2 (Lit i) j = i + j


data T a where
  D1 :: Int -> T String
  D2 :: T Bool
  D3 :: (a, a) -> T [a]

evalT :: T a -> a
evalT (D1 i) = show i
evalT D2 = False
evalT (D3 (x,y)) = x : y : []

data Parser tok a where
  Zero :: Parser tok ()
  One :: Parser tok ()
  Check :: (tok -> Bool) -> Parser tok tok 
  Satisfy :: ([tok] -> Bool) -> Parser tok [tok]
  Push :: tok -> Parser tok a -> Parser tok a
  Plus :: Parser tok a -> Parser tok b -> Parser tok (Either a b)
  Times :: Parser tok a -> Parser tok b -> Parser tok (a, b)
  Star :: Parser tok a -> Parser tok [a]

parse :: Parser tok a -> [tok] -> Maybe a
parse Zero ts = mzero
parse One []  = return ()
parse One _   = mzero
parse (Check p) [t] = if p t then return t else mzero
parse (Check p) _   = mzero
parse (Satisfy p) xs = if p xs then return xs else mzero
parse (Push t x) ts = parse x (t:ts)
parse (Plus x y) ts = liftM Left (parse x ts) `mplus` liftM Right (parse y ts)
parse (Times x y) [] = liftM2 (,) (parse x []) (parse y [])
parse (Times x y) (t:ts) =
  parse (Times (Push t x) y) ts `mplus` liftM2 (,) (parse x []) (parse y (t:ts))
parse (Star x) [] = return []
parse (Star x) (t:ts) = do
  (v,vs) <- parse (Times x (Star x)) (t:ts)
  return (v:vs)


token x = Check (== x)
string xs = Satisfy (== xs)

p = Times (token 'a') (token 'b')
p1 = Times (Star (token 'a')) (Star (token 'b'))
p2 = Star p1

blocks :: (Eq tok) => Parser tok [[tok]]
blocks = Star (Satisfy allEqual)
    where allEqual xs = and (zipWith (==) xs (drop 1 xs))

evenOdd = Plus (Star (Times (Check even) (Check odd)))
               (Star (Times (Check odd) (Check even)))


