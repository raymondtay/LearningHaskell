
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)
type ClockTime = UTCTime

type Predicate = FilePath -- path to directory entry
        -> Permissions    -- permissions
        -> Maybe Integer  -- file size
        -> ClockTime      -- last modified
        -> Bool
-- The return type pf Predicate is Bool and not IO Bool => pure function w/o I/O.

getFileSize :: FilePath -> IO (Maybe Integer)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind predicate path = getRecursiveContents path >>= filterM check
    where check name = do
	        perms    <- getPermissions name
	        size     <- getFileSize name
	        modified <- getModificationTime name
	        return (predicate name perms size modified)

-- `filterM` behaves like the normal filter function, but in this case
-- it evaluates the predicate in the IO monad, allowing the predicate
-- to perform I/O. 

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h    <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\_ -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

{-
    The bracket function takes three actions as arguments. The first action acquires a 
    resource. The second releases the source. The third runs in between, while the resource
    is acquired; let's call this the "use" action. If the "acquire" action succeeds, the 
    "release" action is always called. This guarantees that the resource will always be released.
    The "use" and "release" actions are each passed the resource acquired by the "acquire" action.
-}
getFileSize path = handle (\_ -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

type InfoP a = FilePath
        -> Permissions
        -> Maybe Integer
        -> ClockTime
        -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

-- we like to be able to write other binary functions. we'd prefer not to write
-- a complete definition of each one, because that seems unnecessarily verbose.
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>) 
lesserP  = liftP (<)

-- we can combine predicates by applying the concept of `lifting`
-- 
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2(&&)
orP  = liftP2(||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

-- In haskell, functions that take other functions as arguments
-- and return new functions as "combinators"

liftP' q f k w x y z = f w x y z `q` constP k w x y z

myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

-- more combinators using lifting
-- question: how do you decide the precedence? based on page 225-226 of the book
--           we can discover the precedence level by uncovering the precedence level of the 
--           existing operators (if any) and apply; of course its largely dependent on whether 
--           the new operators( defined via lifting) makes sense in the targeted expressions
(==?) = equalP
(&&?) = andP
(||?) = orP
(>?)  = greaterP
(<?)  = lesserP
infix 4 ==?
infixr 3 &&?
infixr 3 ||?
infix 4 >?

-- example
myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

