import qualified Control.Monad.Writer as W
import Data.Monoid
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


class MonadA m where
    returnA :: a -> m a  -- looks a lot like `pure` in Applicatives
    (=>>=) :: m a -> (a -> m b) -> m b

    (=>>) :: m a -> m b -> m b
    x =>> y = x =>>= \_ -> y 

    failA :: String -> m a
    failA msg = error msg

data Option a = JustO a | NothingO deriving (Eq, Show)

instance MonadA Option where
    returnA x = JustO x
    NothingO =>>= f = NothingO
    (JustO a) =>>= f = f a
    failA _ = NothingO

-- 
-- The following can be achieved now :
-- > NothingO =>> JustO 4 =>> JustO 5
-- > NothingO
-- > JustO 4 =>> JustO 5
-- > JustO 5
--

{-
 Let's create a writer following up the previous idea.
 Don't fear iteration because it's a good thing :-) and so...
 the general idea is to transform the `applyLog` idea into a Monad-ic like structure
 and we have the following 
 where we declare a new type called `WriterA` which takes in a `w` and `a` 
 where `w` is actually a Monoid and `a` represents the type of the value which we wish to extract
 and bind to.
 
 The `runWriterA` is there to replace the behavior of deriving from `Show`.
-}

newtype WriterA w a = WriterA { runWriterA :: (a, w) }
instance (Monoid w) => MonadA (WriterA w) where
    returnA x = WriterA (x, mempty)
    (WriterA (x, y)) =>>= f = let (WriterA (y'', y')) = f x in WriterA (y'', y `mappend` y')

-- with the new defn of `logNumber` we can accomplish something like this
-- > runWriter $ logNumber 5
-- > (5, "Got number: 5")
--
logNumber x = W.writer (x, ["Got number: " ++ show x])

-- With the following `do` notation, we can write an expression like this 
-- > runWriter $ multWithLog
-- > (12, ["Got number: 3", "Got number: 4"])
-- and we noticed that the 
multWithLog :: W.Writer [String] Int
multWithLog = do
    x <- logNumber 3
    y <- logNumber 4
    W.tell ["going to multiply two numbers"]
    return (x * y)

-- With the following `do` notation, we can write an expression like this 
-- > runWriter $ multWithLogTell
-- > (12,["Got number: 3","Got number: 4","going to multiply two numbers"])

multWithLogTell :: W.Writer [String] Int
multWithLogTell = do
    x <- logNumber 3
    y <- logNumber 4
    W.tell ["going to multiply two numbers"]
    return (x * y)


-- If we take the idea a little further and attempt to understand 
-- how the greatest-common-divisor algorithm worked, we can use the Writer Monad
-- and uncover its darkest secret by tracing every step of the algorithm via the
-- `W.tell` ... the only concern i have is whether its dependent on the computation
-- having a deterministic nature to it. Since the `computation` and the `logs` are 
-- pretty intertwined with one another.

gcd' :: Int -> Int -> W.Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        W.tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        W.tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  

