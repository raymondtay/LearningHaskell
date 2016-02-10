module Pointfree where

{-
 - It is very common for functional programmers to write functions as a composition of other
 - functions, never mentioning the actual arguments they will be applied to. 
 - 
 -}

sum :: (Num b, Foldable t) => t b -> b
sum = foldr (+) 0

{-
 - The above is completely equivalent to 
 - sum' xs = foldr(+) 0 xs
 - but the former is more compact. This is very much related to function pipelines 
 - and its, debatably, clearer to write 
 - f . g . h than to write, say let fn x = f (g ( h x ))
 -}

roundtrip :: (Read a, Show a) => a -> a
roundtrip a = read (show a)

{- another example of pointfree style -}

inc = (1+)
three = inc . inc . inc

