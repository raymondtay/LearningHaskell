{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Text.Show.Pretty (ppShow) -- package "pretty-show" is needed 

--
-- The logic evaluating, splicing, and introspecting compile-time values is
-- embedded within the Q monad, which has a runQ which can be used to evaluate
-- its context. These functions of this monad is deeply embedded in the
-- implementation of GHC.
--

aExpr :: Q Exp -- expression example
aExpr = [e| \x -> x |]

bExpr :: Q [Dec] -- quasiquoter for top-level declarations, top-level
bExpr = [d| data Nat = Z | S Nat |]

-- cExpr :: Q Exp
-- cExpr = [p| S (S Z) |]

dExpr :: Q Type
dExpr = [t| Int -> [Int] |] 


g :: p -> p
g = $(runQ [| \x -> x |]) -- g 3 would return 3

-- This declaration builds the function (f = \(a,b) -> a) and TemplateHaskell
-- has its internal DSL to define the AST.
f :: Q [Dec]
f = do
  let f = mkName "f"
  a <- newName "a"
  b <- newName "b"
  return [FunD f [Clause [TupP [VarP a , VarP b]] (NormalB (VarE a)) [] ]]

my_id :: a -> a
my_id x = $([| x |])

introspect :: Name -> Q Exp
introspect n = do
  t <- reify n
  runIO $ putStrLn $ ppShow t
  [| return () |]


