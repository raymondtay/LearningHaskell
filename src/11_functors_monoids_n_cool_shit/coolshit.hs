
module CoolShit where -- frankly, since when is shit EVER ever cool? 

{-

class Functor f => Applicative (f :: * -> *) where
  ...
  (<*>) :: f (a -> b) -> f a -> f b
  ...
    -- Defined in ‘Control.Applicative’
infixl 4 <*>


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

"pure" puts a value into a default context and if we just put a function in a
default context and then extract and apply it to a value inside another applicative functor,
we did the same as just mapping that function over that applicative functor. Instead of 
writing 
> pure f <*> x <*> y <*> ... 
we can write 
> fmap f x <*> y <*> ...
and we can alternatively use the <$> provided by the Control.Applicative 

> (<$>) :: Functor f => (a -> b) -> f a -> f b
>       g <$> x = fmap g x
>     -- Defined in `Data.Functor'
> infixl 4 <$>

Before i proceed any further, i want to remind myself (and possibly yourself) that 
type variables are independent of parameter names or other value names. The 
"f" in the function declaration here is a type variable with a class constraint saying that
any type constructor that replaces "f" should be in the "Functor" typeclass. The "f" in the function
body denotes a function that we map over "x". The fact that we used "f" to represent both
of those doesn't mean that they somehow represent the same thing .

Using <$>, the applicative style shines through because if we want to apply a function "f" between 
three applicative functors, we can write f <$> x <*> y <*> z. If the parameters were not applicative
functors but normal values, we would write "f x y z".

Turns out that there are other applicative functos and one of them is the list type constructor
> instance Applicative [] where
>    pure x = [x]
>    gs <*> hs = [f g | g <- gs, h <- hs]
-}

data SM a = Rubbish | Mightbe a deriving (Show)
instance Functor SM where
    fmap f Rubbish = Rubbish
    fmap f (Mightbe x) = Mightbe (f x)

{-
example of how IO as a Functor is declared
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
-}
