
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


import Antiquote

-- What is interesting, for me, is that it looks like a regular function
-- definition but what is special is that the un-templating mechanism is
-- triggered i.e. "a <snippet> = <x>" where the snippet actually is conducting
-- pattern-matching and here's an example:
--
-- a Zero               => pattern matching failed
-- a (Succ Zero)        => will give Zero
-- a (Succ (Succ Zero)) => will give Succ Zero 
-- 
-- that's probably why its called "anti-quoting"
--
a :: Expr -> Expr
a [mini| succ $x|] = x

-- Now, this function over here is more predictable as it is the typical
-- templating and what it does is to 
f :: Integer -> Expr
f x = [mini| succ $x|]

b :: Expr -> Expr
b [mini|succ $x |] = [mini|pred $x|]

c :: Expressible a => a -> Expr
c x = [mini| succ $x|]

d :: Expr
d = c (8 :: Integer) -- the type annotation is superfluous, fyi.

e :: Expr
e = c True

main :: IO ()
main = do
  putStrLn . show $ a (Succ Zero)
  putStrLn . show $ a (Succ (Succ Zero))
  putStrLn . show $ f 8
  putStrLn . show $ b $ a (Succ (Succ Zero))
  putStrLn . show $ c (4 :: Integer)
  putStrLn . show $ d
  putStrLn . show $ e

