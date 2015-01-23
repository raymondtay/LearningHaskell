{-
    When we first talked about functors, we say that they were a useful concept for values that
    can be mapped over. Then, we took that concept one step further by introducing applicative functors, 
    which allowed us to view values of certain data types as values with contexts and use normal functions
    on those values while preserving the meaning of those contexts.

    Monads are beefed up applicative functors, much like applicative functors are only beefed up functors.

    When we started off with functors, we saw that it's possible to map functions over various data types.
    We saw that for this purpose, the `Functor` type class was introduced and it has us asking the question:
    when we have a function of type a -> b and some data type f a, how do we map that function over the 
    data type to end up with f b? 

    We saw how to map something over a Maybe a, a list [a], an IO a etc We even saw how to map a function
    a -> b over other functions of type r -> a to get functions of type r -> b. To answer this question
    of how to map a function over some data type, all we had to do was look at the type of fmap:

    fmap :: (Functor f) => (a -> b) -> f a -> f b

    and make it work for our data type by writing the appropriate Functor instance.


    Monads are a natural extension of applicative functors and with them we are concerned with this.
    If you have a value with a context like `m a` and how do you apply it to a function that takes a normal
    a and returns a value with a context? That is how do you apply a function of type a -> m b to a value
    of type m a ? So essentially we will want something like the following :

    (something) :: (Monad m) => m a -> (a -> m b) -> m b

    but we don't know what is `something` and we can try a new symbol 
    
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-}

-- 
-- As a primer, let's look at how we might possibly extract 
-- a value from a context like `Maybe x` and with that, we can 
-- actually do something like the following:
-- > applyM (Just 4) (\x -> x + 42)
-- > Just 46
-- 
applyM Nothing f = Nothing
applyM (Just x) f = Just (f x)

-- 
-- The Monad type class
-- Just like functions have the `Functor` type class and applicative functors have the `Applicative`
-- type class, monads comes with their own types
-- 
class MonadT m where
    returnT :: a -> m a  -- looks a lot like `pure` in Applicatives
    (=>>=) :: m a -> (a -> m b) -> m b

    (=>>) :: m a -> m b -> m b
    x =>> y = x =>>= \_ -> y 

    failT :: String -> m a
    failT msg = error msg

data Option a = JustO a | NothingO deriving (Eq, Show)

instance MonadT Option where
    returnT x = JustO x
    NothingO =>>= f = NothingO
    (JustO a) =>>= f = f a
    failT _ = NothingO

-- 
-- The following can be achieved now :
-- > NothingO =>> JustO 4 =>> JustO 5
-- > NothingO
-- > JustO 4 =>> JustO 5
-- > JustO 5
--

