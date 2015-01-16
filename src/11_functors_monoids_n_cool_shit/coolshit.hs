
module CoolShit where -- frankly, since when is shit EVER ever cool? 

{-

class Functor f => Applicative (f :: * -> *) where
  ...
  (<*>) :: f (a -> b) -> f a -> f b
  ...
    -- Defined in ‘Control.Applicative’
infixl 4 <*>

-}

-- Turns out that <*> is left-associative ! cool !
-- So let's use the following example to understand what <*> actually does ?
when we say pure (+) <*> Just 3 <*> Just 5
what actually happens is that <*> is actually left-associative which means the above
expression becomes 
> (pure (+) <*> Just 3) <*> Just 5
> (Just (+) <*> Just 3) <*> Just 5
> (Just (+3)) <*> Just 5
> (Just (5+3)) 
> Just 8

Applicative functors and the applicative style of doing
> pure f <*> x <*> y <*> ... 
allows us to take a function that expects parametrs that aren't necessarily
wrapped in functors and use that function to operate on several values
that are in functor contexts. The function can take as many parameters as we want,
because it's always partially applied step by step between occurences of <*>














