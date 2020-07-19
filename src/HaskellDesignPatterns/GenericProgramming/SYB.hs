{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module SYB where

import Data.Typeable

-- Abbreviation for Scrap-Your-Boilerplate
-- * The original paper was in 2003 via Ralf Lammel, Simon Peyton Jones
-- * The sequel paper is entitled "Scrap Your Boilerplate - Reloaded" in 2013 via Ralf Hinze et al
-- A design pattern for writing programs that traverseSYB data structures built
-- from rich mutually-recursive data types.
--
-- [FYI] : The hackage package [syb](http://hackage.haskell.org/package/syb)
-- implements the combinators library listed in the paper. The package has
-- about 300k+ downloads till today (i.e. 11 July 2020) so it means its a
-- popular package over the years ☺

-- The solution is to discover a general solution to traversal rich and
-- recursive data types.
--
data Company  = C [Dept] deriving (Show)
data Dept     = D Name Manager [SubUnit] deriving (Show)
data SubUnit  = PU Employee | DU Dept deriving (Show)
data Employee = E Person Salary deriving (Show)
data Person   = P Name Address deriving (Show)
data Salary   = S Float deriving (Show)
type Manager  = Employee
type Name     = String
type Address  = String

generateCoy :: Company
generateCoy = C [D "Research" ralf [PU joost, PU marlow], D "Strategy" blair []]

ralf, joost, marlow , blair :: Employee
ralf   = E (P "Ralf"  "Amsterdam") (S 8000)
joost  = E (P "Joost"  "Amsterdam") (S 1000)
marlow = E (P "Marlow"  "Cambridge") (S 2000)
blair  = E (P "Blair"  "London") (S 100000)

increaseSalary :: Float -> Company -> Company
increaseSalary d (C ds) = C (map (incD d) ds)

-- The following functions incD, incU, incE, incS are examples of what is meant
-- by boilerplate because its a routine traversal of a tree structure
incD :: Float -> Dept -> Dept
incD k (D nm mgr us) = D nm (incE k mgr) (map (incU k) us)

incU :: Float -> SubUnit -> SubUnit
incU k (PU e) = PU (incE k e)
incU k (DU d) = DU (incD k d)

incE :: Float -> Employee -> Employee
incE k (E p s) = E p (incS k s)

incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

-- I want to write a more generic form of `increaseSalary`
-- increaseSalaryGen :: Float -> Company -> Company
-- increaseSalaryGen k = everywhere (mkT (incS k))

-- The function "incr" means that as long as its-a Typeable, then the function
-- can happily apply a salary increment.
incr :: (Typeable a) => Float -> a -> a
incr k = mkT (incS k)

-- gmapT and gmapQ represents a gen
class Typeable a => Term a where
  gmapT :: (forall b . Term b => b -> b) -> a -> a
  gmapQ :: (forall b . Term b => b -> r) -> a -> [r]
  gmapM :: (Monad m) => (forall b . Term b => b -> m b) -> a -> m a

instance Term Company where
  gmapT f (C depts) = C (f depts)
  gmapQ f (C depts) = [f depts]
  gmapM f (C depts) = do d <- f depts
                         return (C d)

instance Term Dept where
  gmapT f (D name mgr subunits) = D (f name) (f mgr) (f subunits)
  gmapQ f (D name mgr subunits) = [f name] ++ [f mgr] ++ [f subunits]
  gmapM f (D name mgr subunits) = do n <- f name
                                     m <- f mgr
                                     su <- f subunits
                                     return (D n m su)

instance Term SubUnit where
  gmapT f (PU employee) = PU (f employee)
  gmapT f (DU dept)     = DU (f dept)
  gmapQ f (PU employee) = [f employee]
  gmapQ f (DU dept)     = [f dept]
  gmapM f (PU employee) = do e <- f employee
                             return (PU e)
  gmapM f (DU dept) = do d <- f dept
                         return (DU d)

instance Term Employee where
  gmapT f (E per sal) = E (f per) (f sal)
  gmapQ f (E per sal) = [f per] ++ [f sal]
  gmapM f (E per sal) = do p' <- f per
                           s' <- f sal
                           return (E p' s')

instance Term Person where
  gmapT f (P name addr) = P (f name) (f addr)
  gmapQ f (P name addr) = [f name] ++ [f addr]
  gmapM f (P name addr) = do n' <- f name
                             a' <- f addr
                             return (P n' a')

instance Term Salary where
  gmapT f (S s) = S (f s)
  gmapQ f (S s) = [f s]
  gmapM f (S s) = do a <- f s
                     return (S a)

instance Term Float where
  gmapT f n = n
  gmapQ f _ = []
  gmapM f n = return n

instance Term Char where
  gmapT f n = n
  gmapQ f _ = []
  gmapM f n = return n

instance Term a => Term [a] where
  gmapT f [] = []
  gmapT f (x:xs) = f x : xs
  gmapQ f [] = []
  gmapQ f (x:xs) = [f x , f xs]
  gmapM f [] = return []
  gmapM f (x:xs) = do a <- f x
                      b <- f xs
                      return (a:b)
-- gmapT only applies to the immediate children of the node as opposed to any
-- kind of recursive traversal. i.e. "shallow traversal"
-- gmapQ applies

-- traverseSYB is a recursive definition which applies 'f' to 'x' and it goes
-- all the way to uncover the structure. The way to read this is that it
-- starts from the top and proceeds forward till it reaches its children etc
-- and applies 'f' and rolls back till the top. Bottom-up approach
traverseSYB :: Term a => (forall b. Term b => b -> b) -> a -> a
traverseSYB f x = f (gmapT (traverseSYB f) x)

-- The difference is that top-down approach vs the bottom-up approach.
traverseSYB' :: Term a => (forall b. Term b => b -> b) -> a -> a
traverseSYB' f x = gmapT (traverseSYB f) (f x)

increaseSalaryGen :: Float -> Company -> Company
increaseSalaryGen k = traverseSYB (mkT (incS k))

traverseSYB'' :: Term a => (r -> r -> r) -> (forall a. Term a => a -> r) -> a -> r
traverseSYB'' k f x = foldl k (f x) (gmapQ (traverseSYB'' k f) x)

traverseM :: (Monad m, Term a) => (forall b. Term b => b -> m b) -> a -> m a
traverseM f x = gmapM (traverseM f) x >>= f

billS :: Salary -> Float
billS (S s) = s

allSalaries :: Company -> Float
allSalaries = traverseSYB'' (+) (0 `mkQ` billS)

mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = case cast f of
  Just g  -> g
  Nothing -> id

-- With the advent of this new combinator, traverseSYB'', we can built other
-- combinators with it and one such HoF is `find` which attempts to locate a
-- department given its name, within a company.
find :: Name -> Company -> Maybe Dept
find name = traverseSYB'' orElse (Nothing `mkQ` findD name)

findD :: String -> Dept -> Maybe Dept
findD name d@(D name' _ _)
  | name == name' = Just d
  | otherwise = Nothing


orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
  Just _ -> x
  Nothing -> y


-- mkQ represents a query and behaves as follows when applied to an argument
-- 'a': if a's type is the same as q's argument type, use q to interrogate a;
-- otherwise return the default value r.
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(mkQ r q) a = case cast a of 
  Just a' -> q a'
  Nothing -> r

-- mkM represents a monadic query and its not that different from mkQ or even
-- mkT. 
mkM :: (Typeable a, Typeable b,
        Monad m, Typeable (m a) ,
        Typeable (m b)) => (b -> m b) -> a -> m a
mkM f = case cast f of
  Just g -> g
  Nothing -> return

main :: IO ()
main = do
  putStrLn "→ Total salaries: "
  putStrLn . show $ allSalaries generateCoy
  --- 
  putStrLn "→ After applying an additional 20% increment:"
  putStrLn . show $ increaseSalaryGen 0.2 generateCoy -- works! leaner approach
  putStrLn . show $ increaseSalary    0.2 generateCoy -- works as expected, has more boilerplate code
  --- 
  putStrLn "→ Location department: Strategy"
  putStrLn . show $ find "Strategy" generateCoy
  putStrLn "→ Location department: Research"
  putStrLn . show $ find "Research" generateCoy


