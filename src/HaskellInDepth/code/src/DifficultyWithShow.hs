
data Expr a = Lit a
    | Add (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)


expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)

expr2 = Add (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2))
              (Add (Lit 2)
                (Mult (Lit 2) (Add (Lit 1) (Lit 2))))))
                (Add (Lit 1) (Mult (Lit 3) (Lit 2)))

-- The small problem with this approach is that the nuts & bolts are closely
-- linked. Recursion-schemes might be a better approach to solving this problem
-- in a more elegant manner i.e. holistic approach imo.
--
instance Show a => Show (Expr a) where
  showsPrec _ (Lit a) = shows a
  showsPrec p (Add e1 e2) = showParen (p > precAdd)
                            $ showsPrec precAdd e1 . showString "+" . showsPrec precAdd e2
                              where precAdd = 5
  showsPrec p (Mult e1 e2) = showParen True -- (p > precMult)
                            $ showsPrec precMult e1 . showString "*" . showsPrec precMult e2
                              where precMult = 6

-- This sort of problem is already solved by recursion-schemes i.e.
-- catamorphism
myeval :: Num a => Expr a -> a
myeval (Lit e) = e
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2

