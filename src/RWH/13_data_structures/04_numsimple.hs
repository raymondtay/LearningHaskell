
import Data.List

--------------------------------------
-- Symbolic / Units manipulation
--------------------------------------



data Op = Plus | Minus | Mul | Div | Pow deriving (Show, Eq)

{- The core symbolic manipulation type -}
data SymbolicManip a = Number a | Arith Op (SymbolicManip a) (SymbolicManip a) deriving (Show, Eq)

{- SymbolicManip will be an instnace of Num. Define how the Num operations are
handled over a SymbolicManip. This will implement things like (+) for SymbolicManip. -}

instance Num a => Num (SymbolicManip a) where
   a + b = Arith Plus a b
   a - b = Arith Minus a b
   a * b = Arith Mul a b 
   negate a = Arith Mul (Number (-1)) a
   abs a = error "abs is unimplemented"
   signum _ = error "signum is unimplemented"
   fromInteger i = Number (fromInteger i)

{- Make SymbolicMainp an instance of Fractional -}
instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a 
  fromRational r = Number (fromRational r) 


{- Make SymbolicManip an instance of Floating -}
instance (Floating a) => Floating (SymbolicManip a) where
  pi = Symbol "pi"
  exp a = UnaryArith "exp" a
  log a = UnaryArith "log" a
  sqrt a = UnaryArith "sqrt" a
  a ** b = BinaryArith Pow a b
  sin a = UnaryArith "sin" a
  cos a = UnaryArith "cos" a
  tan a = UnaryArith "tan" a

{- Show a SymbolicManip as a String, using convention algebraic notation -}
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) = 
  let pa = simpleParen a
      pb = simpleParen b
      pop = op2str op
  in pa ++ pop ++ pb

prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

{- Add parentheses where needed. This function is fairly conservative and will
   add parenthesis when not needed in some cases.
   Haskell will have already figured out precedence for us while building up the SymbolicManip.
-}

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _ ) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
  show = prettyShow
