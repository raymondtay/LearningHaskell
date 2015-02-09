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
-- An example would be the following:
-- > gcd' 1212 773
-- > (1,["1212 mod 773 = 439",
-- >      "773 mod 439 = 334",
-- >      "439 mod 334 = 105",
-- >      "334 mod 105 = 19",
-- >       "105 mod 19 = 10",
-- >        "19 mod 10 = 9",
-- >         "10 mod 9 = 1",
-- >          "9 mod 1 = 0",
-- >     "Finished with 1"])

gcd' :: Int -> Int -> W.Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        W.tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        W.tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  

-- With the above expression, we can actually put the `logs` 
-- by the following machination like
-- > snd $ runWriter (gcd' 888 13)
-- which would return us 
-- > ["888 mod 13 = 4","13 mod 4 = 1","4 mod 1 = 0","Finished with 1"]
-- but that's really a type of 
-- > snd $ runWriter (gcd' 888 13) :: [String]
-- and i thought i could lift out the data in the list by using 
-- > map putStrLn $ snd $ runWriter (gcd' 888 13) 
-- but that turns out to be of type [IO ()] <- Voila !!!! its a IO Monad !
-- > :t map putStrLn $ snd $ runWriter (gcd' 888 13) :: [IO ()]
-- So we revert to using `mapM_` instead and we lift out the expression 
-- > :t mapM_ putStrLn $ snd $ runWriter (gcd' 888 13) :: IO ()
-- and we have the lovely printout :
-- > 888 mod 13 = 4
-- > 13 mod 4 = 1
-- > 4 mod 1 = 0
-- > Finished with 1
-- 


{-
 The interesting fact coming up next, accoridng to the book, is the fact that
 list creation in certain use cases are bound to be slower and there's a way to fix
 that.
-}

gcdReverse :: Int -> Int -> W.Writer [String] Int
gcdReverse a b 
    | b  == 0 = do
        W.tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        W.tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

{- Difference List -}
{-
 The following definitions allows me to define 
 > fromDiffList $ toDiffList [1..10] `mappend` toDiffList [11..15]
 > [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
 and finally the expression like the following
 > mapM_ putStrLn $ fromDiffList $ snd $ runWriter $ gcdDiffList' 888 13
 > 888 mod 13 = 4
 > 13 mod 4 = 1
 > 4 mod 1 = 0
 > Finished with 1
-}

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList l) `mappend` (DiffList r) = DiffList (\xs -> l (r xs))

gcdDiffList' :: Int -> Int -> W.Writer (DiffList String) Int  
gcdDiffList' a b  
    | b == 0 = do  
        W.tell (toDiffList ["Finished with " ++ show a])
        return a  
    | otherwise = do  
        W.tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        gcdDiffList' b (a `mod` b)  

finalCountDown :: Int -> W.Writer (DiffList String) ()
finalCountDown 0 = do
    W.tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x - 1)
    W.tell (toDiffList [show x])

{-
 Composing Monadic Functions
 
 Like any kind of functions in haskell, we can compose two (or more) functions together to reach 
 a new kind of functionality
 > let f = (+1) . (*100)
 and we can do the same with monads too. here's how:
 > let g = (\x -> return (x+1)) <=< (\x -> return (x*100))
 when we run the two functions like this:
 > g(3) or f(3)
 they all return the same result which is 301

 Here's another great way to compose monads 
 *Main> let h = foldr (.) id [(+1), (*100), (+1)]
 *Main> h(1)
 201
 *Main> let i = (\x -> return (x+1)) <=< (\x -> return (x*100)) <=< (\x -> return (x+1))
 *Main> i(1)
 201

 Another way to compose monads is thru the "convenience" function '<=<'
 below is an example of how we can possibly use this.
 Prelude Control.Monad> :t (<=<)
 (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
 Prelude Control.Monad> let f x = [x,-x]
 Prelude Control.Monad> let g x = [x*3, x+4]
 Prelude Control.Monad> :t f
 f :: Num t => t -> [t]
 Prelude Control.Monad> f <=< g $ 5
 [15,-15,9,-9]
 Prelude Control.Monad>

-}

