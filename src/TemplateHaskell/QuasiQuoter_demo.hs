{-# LANGUAGE QuasiQuotes #-}

--
-- See the module Quasiquote.hs for the details of the implementation
--
-- Note: GHC 8.6.5 is good enough to run this example, but i'm not sure how
-- backward compatible it is going to be.
--
--
import Quasiquote

a :: Expr
a = [calc|true|]

b :: Expr
b = [calc| succ (succ 0)|]

c :: Expr 
c = [calc| pred (succ 0)|]


main :: IO ()
main = do
  putStrLn . show $ a
  putStrLn . show $ b
  putStrLn . show $ c

