
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

