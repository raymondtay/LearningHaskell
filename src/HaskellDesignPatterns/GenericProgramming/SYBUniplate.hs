{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Direct


data Expr = Var Int | Neg Expr | Add Expr Expr

instance Uniplate Expr where
  uniplate (Var x) = plate Var |- x
  uniplate (Neg x) = plate Neg |* x
  uniplate (Add a b) = plate Add |* a |* b

-- eval :: Expr -> Expr 
eval = transform b
  where
    b (Var x) = x
    b (Neg x) = negate x
    b (Add x y) = (b x) + (b y)

main :: IO ()
main = do
  return ()

  -- putStrLn . show $ descend (1+) (Add (Var 2) (Var 4))

