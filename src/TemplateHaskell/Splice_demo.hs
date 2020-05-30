{-# LANGUAGE TemplateHaskell #-}

-- 
-- At the point of the splice, all variables and types used must be in scope,
-- so it must appear after their declarations in the module. As a result, we
-- have to mentally and topologically sort our code when using TemplateHaskell
-- such that declarations are defined in order.
--
import Splice

spliceF 
spliceG "argument"

main = do
  print $ f 1 2 -- prints "1"
  print $ f 3 4 -- prints "3"
  g 3.142
  g ()


