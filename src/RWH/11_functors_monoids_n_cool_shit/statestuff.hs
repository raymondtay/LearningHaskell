import Control.Monad.State
import Control.Monad.Writer
import System.Random
import Control.Monad.Writer

type Stack = [Int]

pop :: State Stack Int -- pop takes a state and places the first element in the first position of a 2-tuple
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack () -- push takes a number and prepends it to the current stack
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

{- 
  Another way to write `stackManip` in the above 
  is to make use of the monad-chaining function (>>=)
  which allows us to chain computations, literally.
  However, to run this brand new function is the same as 
  running `stackManip` which is
  runState stackManipVerbose [1,2,3] --- new function
  runState stackManip [1,2,3] ---------- old function
-}
stackManipVerbose = push 3 >>= (\_ -> pop) >>= (\_ -> pop)

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
  then 
    push 5
  else do
    push 3
    push 8

getS = state $ \s -> (s,s)
putS newstate = state $ \_ -> ((), newstate)

anotherStackStuff :: State Stack ()
anotherStackStuff = do
    now <- getS
    if now == [1,2,3]
    then putS [3,2,1]
    else putS [4,5,6]

{-

Here's how we could have written a State-Monad instance
and we start off by defn a new type `StateA` and from there
we are going to define how we can interact with this state monad

newtype StateA s a = StateA { runState :: s -> (a, s) }
instance Monad (StateA x) where
    return x = StateA $ \s -> (x,s)
    (StateA l) >>= f = StateA $ \s -> let (a, newState) = l s
                                          (StateA r) = f a
                                      in r newState

-}

randomCoinThrow = state random

throw3 :: State StdGen (Bool,Bool,Bool)
throw3 = do
    a <- randomCoinThrow
    b <- randomCoinThrow
    c <- randomCoinThrow
    return (a,b,c)

{-
 Here's another way to re-write the above `throw3`
 via monad-chaining 
 and we can check that it works by executing the former
 and the latter in succession like this:

  *Main Control.Monad.State> runState throw3Verbose (mkStdGen 33)
  ((True,False,True),680029187 2103410263)
  *Main Control.Monad.State> runState throw3 (mkStdGen 33)
  ((True,False,True),680029187 2103410263)
  *Main Control.Monad.State>

-}
throw3Verbose = (randomCoinThrow >>= (\r1 -> randomCoinThrow >>= (\r2 -> randomCoinThrow >>= (\r3 -> return (r1, r2, r3)))))

{-
Some useful monadic functions

When we started our journey to the top of Monad mountian,
we first looked at `functors`, which are for thigns that can be mapped over.
Then, we learned about improved functors i.e. `applicative functors` which allows us to 
apply normal funcitons between several applicative values as well as to 
take a normal value and put it in some default context. Finally,
we introduced Monads as improved applicative functors, which added the ability for 
these values with context to somehow be fed into normal funcitons.

So every monad is an applicative functor and every applicative functor is afunctor.
The `Applicative` typeclass has a class constraint such that our type has to be an 
instance of `Functor` before we can make it an instance of `Applicative'. But even 
though `Monad` should have the same constraint for `Applicative`, as every monad is an applicative
functor, it doesn't, because the Monad tpe class was introduced to haskell way before `Applicative`.

But even though every monad is a functor, we don't have to rely on it having a `Functor` instance because of the `liftM`
function. `liftM` takes a function and a monadic value and maps it over the monadic value. 
-}

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
    | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

{-
 Now we can write something like 
 ```
 runWriter (keepSmall 3)
 Loading package array-0.5.0.0 ... linking ... done.
 Loading package deepseq-1.3.0.2 ... linking ... done.
 Loading package old-locale-1.0.0.6 ... linking ... done.
 Loading package time-1.4.2 ... linking ... done.
 Loading package random-1.0.1.1 ... linking ... done.
 Loading package transformers-0.3.0.0 ... linking ... done.
 Loading package mtl-2.1.3.1 ... linking ... done.
 (True,["Keeping 3"])
 *Main> runWriter (keepSmall 3)
 ```
 Or we can write something like
 ```
 *Main> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [1,2,12,13,14]
 Keeping 1
 Keeping 2
 12 is too large, throwing it away
 13 is too large, throwing it away
 14 is too large, throwing it away
 *Main>
 ```
-}

dummy = let 
  f = (+2)
  g = (*5)
  h = (*3) in fmap f (\x -> fmap g h x) 5

{-
 With the following definition, we can write an expression like
 > mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [1,2,3,4]
 > Keeping 1
 > Keeping 2
 > Keeping 3
 > 4 is too large, throwing it away
-}

ff :: Int -> Writer [String] (Int,Int)
ff = (\x -> 
  do
  tell ["tuple: " ++ show(x,x)]
  return (x,x))

{- let's do a powerset function -}
powerset xs = filterM (\x -> [True, False]) xs

{- 
1 thing i learnt from monads in haskell is that the function-composition
names actually do tell you the direction of application and this is evident
from the following expressions involving 'gg', 'hh', 'ii', 'jj'
and their applications in 'result_1' and 'result_2'.
-}

gg = (\x -> return (x+10))
hh = (\x -> return (x*2))
ii = gg <=< hh
jj = gg >=> hh
result_1 = Just 4 >>= ii
result_2 = Just 4 >>= jj

{-
 With the following, we can write something like
 > foldM aSmall 0 [1,2,3,121]
 > Nothing -- that's because 121 is > 9
 but however, we can do this
 > foldM aSmall 0 [1,2,3,2]
 > Just 8
-}
aSmall :: Int -> Int -> Maybe Int
aSmall acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)

{-

MonadWriter is defined as follows:

class (Monoid w, Monad m) =>
      MonadWriter w (m :: * -> *)
      | m -> w where
  writer :: (a, w) -> m a
  tell :: w -> m ()
  listen :: m a -> m (a, w)
  pass :: m (a, w -> w) -> m a

-}

