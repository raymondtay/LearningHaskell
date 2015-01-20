
module CoolShit where -- frankly, since when is shit EVER ever cool? 

{-

class Functor f => Applicative (f :: * -> *) where
  ...
  (<*>) :: f (a -> b) -> f a -> f b
  ...
    -- Defined in ‘Control.Applicative’
infixl 4 <*>


Turns out that <*> is left-associative ! cool !
So let's use the following example to understand what <*> actually does ?
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

It would be easier to understand how Haskell's type system works
by trying things out !

> Prelude Control.Applicative> :t (++)
> (++) :: [a] -> [a] -> [a]
> Prelude Control.Applicative> :t pure (++)
> pure (++) :: Applicative f => f ([a] -> [a] -> [a])
> Prelude Control.Applicative> :i pure
=======
Take a look at this example and it should be clear what does the applicatives style
mean when applied to lists
> filter (>50) ( (*) <$> [4,55] <*> [44,22])
> [176,88,2420,1210]
> filter (>50) $ (*) <$> [4,55] <*> [44,22]
> [176,88,2420,1210]

-}
import Control.Applicative

sequenceR :: (Applicative f) => [f a] -> f [a]
sequenceR [] = pure []
sequenceR (x:xs) = (:) <$> x <*> sequenceR xs

{-
> class Functor f => Applicative f where
>   pure :: a -> f a
>   ...
>     -- Defined in `Control.Applicative'

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

For-comprehensions like the following 
> [x*y | x <- [1..10], y <- [1..10]]
can be replaced by applicatives like the following
> (*) <$> [1..10] <*> [1..10]

=> (+) <$> (+3) <*> (*100) $ 5
=> ((+3) + ) <*> (*100) $ 5
=> ((+3) + (*100)) $ 5
=> ((5+4) + (5*100))

=> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5 
=> (\x y z -> [(+3), y, z]) <*> (*2) <*> (/2) $ 5
=> (\x y z -> [(+3), (*2), z]) <*> (/2) $ 5
=> fmap (\x y z -> [(+3), (*2), (/2)]) 5

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
    Applying a similar approach but this time from the right 
    and starting with a `pure []`
-}
sequenceRViaFoldr :: (Applicative f) => [f a] -> f [a]
sequenceRViaFoldr = foldr (liftA2 (:)) (pure [])

{-
    If we wanted to make the tuple an instance of `Functor`
    in such a way that when we `fmap` a function over an
    tuple, it gets applied to the first component of the tuple?
    e.g. `fmap (+3) (1,1)` should return `(4,1)` and turns out
    its rather *difficult* to do that with conventional methods.
    Turns out the `newtype` keyword allows us to circumvent this 
    and this reminds me of the `Type Lambdas` in Scala
-}

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair a) where
    fmap f (Pair (x,y)) = Pair(f x, y)


class MonoidT a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance (MonoidT a) => MonoidT (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    Just a `mappend` Just b = Just (a `mappend` b)

{-
How does `sequenceR [[1,2], [3,4]]` work?
=> sequenceR [[1,2], [3,4]]
=> (:) <$> [1,2] <*> sequenceR [[3,4]]
=> (:) <$> [1,2] <*> ( (:) <$> [3,4] <*> sequenceR [] )
=> (:) <$> [1,2] <*> ( (:) <$> [3,4] <*> [[]] )
=> (:) <$> [1,2] <*> ( [ [3], [4] ] )
=> (:) <$> [1,2] <*> [ [3], [4] ]
=> [ 1:[3], 1:[4], 2:[3], 2:[4] ]
=> [[1,3], [1,4], [2,3], [2,4]]
-}
 
