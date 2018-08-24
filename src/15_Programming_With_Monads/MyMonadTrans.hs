
-- This is a useful Typeclass to use in a production setting for the reasons
-- 
--
module MyMonadTrans where


import Control.Monad.Trans

-- Below are 2 fictitious IO actions which both print some thing and then
-- return a String; the intention is to demonstrate how both v and w can be
-- embedded into another IO action. When you examine the `main` function, you
-- will inherently notice that it looks like `>>` but its not because lftIO
-- allows non-isomorphic expressions to be "chained" together and is definitely
-- different from `>>` which is monomorphic in the type parameter; and its
-- most certainly not `>>=`.
v :: IO String
v = do
  putStrLn "hello"
  return "v"

w :: IO String
w = do
  putStrLn "world"
  return "w"

z :: IO Int
z = do
  putStrLn "world war z"
  return 42

-- The lesson here is two fold:
-- a/ Embedding IO actions in other IO actions
-- b/ The dangers of embedding IO actions can get dangerously uncomfortable.
--
main :: IO ()
main = do
  liftIO v
  liftIO z
  liftIO w
  liftIO z
  return () -- conforming to what `main` type signature needs.

-- How do we address these dangers? The book goes on to support this:
-- The disadvantage of hiding IO in another monad is that we are still tied to
-- a concrete implementation. If we want to swap HandleIO (see HandleIO.hs) for
-- some other monad, we must change the type of every action that uses
-- HandleIO.
-- As an alternative, we can create a typeclass that specifies the interface we
-- want from a monad that manipulates files:
--
