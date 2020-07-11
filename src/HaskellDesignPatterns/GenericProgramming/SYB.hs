{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module SYB where

import Data.Typeable

-- Abbreviation for Scrap-Your-Boilerplate
-- * The original paper was in 2003 via Ralf Lammel, Simon Peyton Jones
-- * The sequel paper is entitled "Scrap Your Boilerplate - Reloaded" in 2013 via Ralf Hinze et al
-- A design pattern for writing programs that traverse data structures built
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

class Typeable a => Term a where
  gmapT :: (forall b . Term b => b -> b) -> a -> a

instance Term Company where
  gmapT f (C depts) = C (f depts)

instance Term Dept where
  gmapT f (D name mgr subunits) = D (f name) (f mgr) (f subunits)

instance Term SubUnit where
  gmapT f (PU employee) = PU (f employee)
  gmapT f (DU dept)     = DU (f dept)

instance Term Employee where
  gmapT f (E per sal) = E (f per) (f sal)

instance Term Person where
  gmapT f (P name addr) = P (f name) (f addr)

instance Term Salary where
  gmapT f (S s) = S (f s)

instance Term Float where
  gmapT f n = n

instance Term Char where
  gmapT f n = n

instance Term a => Term [a] where
  gmapT f [] = []
  gmapT f (x:xs) = f x : xs

-- gmapT only applies to the immediate children of the node as opposed to any
-- kind of recursive traversal. i.e. "shallow traversal"
tfnEverywhere :: Term a => (forall b. Term b => b -> b) -> a -> a
tfnEverywhere f x = f (gmapT (tfnEverywhere f) x)

tfnEverywhere' :: Term a => (forall b. Term b => b -> b) -> a -> a
tfnEverywhere' f x = gmapT (tfnEverywhere f) (f x)

increaseSalaryGen :: Float -> Company -> Company
increaseSalaryGen k = tfnEverywhere (mkT (incS k))


mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = case cast f of
  Just g  -> g
  Nothing -> id

-- mkQ represents a query and behaves as follows when applied to an argument
-- 'a': if a's type is the same as q's argument type, use q to interrogate a;
-- otherwise return the default value r.
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(mkQ r q) a = case cast a of 
  Just a' -> q a'
  Nothing -> r


main :: IO ()
main = do
  putStrLn "After applying an additional 20% increment:"
  putStrLn . show $ increaseSalaryGen 0.2 generateCoy -- works! leaner approach
  putStrLn . show $ increaseSalary    0.2 generateCoy -- works as expected, has more boilerplate code


