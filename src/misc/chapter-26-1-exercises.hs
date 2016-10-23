module Chapter26_1 where

import Control.Monad.Trans.Reader
import Data.Functor.Identity

-- 
-- I'm trying to understand how this would work. From the package's description
-- (https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Monad-Trans-Reader.html), 
-- i see that:
--
-- `type Reader r = ReaderT r Identity` ------------ (1) 
--
-- where `ReaderT` eq `ReaderT r m a` (equivalent to r -> m a)
-- and the key thing to understand here is the function `ask` whose type is
--
-- ask :: Monad m => ReaderT r m r ------------------ (2)
--
-- and as i recall, `ReaderT r m r` is equivalent to `r -> m r`
-- which is in turn equivalent to `Reader a a` and the reason why that's the case
-- is because "Identityt" is defined in Data.Functor.Identity as 
--
-- `newtype Identity a = Identity { runIdentity :: a } deriving (blah blah...)`
-- 
-- and when you work thru the types w.r.t `Reader`, i realized we could write expression (1) 
-- as:
--
-- `type Reader r = ReaderT r (Identity a)` which works out to being `ReaderT (\r -> Identity r)`
-- since that's that `Reader r` really is. Now that i understand what its true form is, its easier 
-- to craft functions for it. Now, i can work my way through ...
--

-- 
-- rDec is a function that should get its argument in the context of Reader
-- and return a value decremented by one.
--
rDec :: Num a => Reader a a
rDec = ReaderT (\a -> Identity $ (subtract 1) a ) 

-- Now, the point free version and now i approach this particular case is the following:
-- (i) first thing i did was to see if i could collapsed the inner lambda and take out the 'a'
--     which gave me the expression `Identity $ subtract 1` which is of type `Identity(a -> a)`
--
-- (ii) next thing i noticed is that `Identity` is really a function so i did a (<$>) which gave
--      this final form. I'm not sure if it could be reduced further but i'm pretty happy at this point.
--       
rDec' :: Num a => Reader a a
rDec' = ReaderT $ (<$>) Identity $ subtract 1 

-- 
-- According to the book, it should be able to perform the two applications with ease:
-- *Chapter26_1 Control.Monad.IO.Class Control.Monad.Trans.Reader Data.Functor.Identity> runReader rDec 1
-- 0
-- *Chapter26_1 Control.Monad.IO.Class Control.Monad.Trans.Reader Data.Functor.Identity> fmap (runReader rDec) [1..10]
-- [0,1,2,3,4,5,6,7,8,9]
--

-- 
-- Now that i have worked out how to do the lifting, its applying the "pattern"
-- to see where it fails and adapt whatever, the end result is my pointfree style
--
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ (<$>) Identity show

-- 
-- According to the book, "rShow" should be able to perform the two applications with ease:
-- *Chapter26_1 Control.Monad.IO.Class Control.Monad.Trans.Reader Data.Functor.Identity> runReader rShow 1
-- "1"
-- *Chapter26_1 Control.Monad.IO.Class Control.Monad.Trans.Reader Data.Functor.Identity> fmap (runReader rShow) [1..10]
-- ["1","2","3","4","5","6","7","8","9","10"]
--

-- rPrintAndInc will first print the input with a greeting, then return 
-- the input incremented by one.
--

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = let
    printConsole :: (Show a, Num a) => a -> IO a
    printConsole a = do putStrLn ("Hi: " ++ show a); return (a+1)
  in ReaderT $ printConsole  
    
-- 
-- According to the book, "rPrintAndInc" should be able to perform the two applications with ease:
-- *Chapter26_1 Control.Monad.IO.Class Control.Monad.Trans.Reader Data.Functor.Identity> runReaderT rPrintAndInc 1
-- Hi: 1
-- 2
-- *Chapter26_1 Control.Monad.IO.Class Control.Monad.Trans.Reader Data.Functor.Identity> traverse (runReaderT rPrintAndInc) [1..10]
-- Hi: 1
-- Hi: 2
-- Hi: 3
-- Hi: 4
-- Hi: 5
-- Hi: 6
-- Hi: 7
-- Hi: 8
-- Hi: 9
-- Hi: 10
-- [2,3,4,5,6,7,8,9,10,11]
--
