{-# LANGUAGE TemplateHaskell #-}

-- The source of this tutorial is found here:
-- https://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html
--
module LensTutorial where

-- The simplest problem that the lens library solves is updating deeply nested
-- records. Suppose you had the following nested Haskell data types:
--
import Control.Lens hiding (element)

data Atom = Atom { _elemt :: String, _point :: Point } deriving Show
data Point = Point { _x :: Double, _y :: Double } deriving Show

-- if i wanted to increase the x coordinate of an `Atom` by one unit, i would
-- have to write something like this:

shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)

-- This unpacking and repacking of data types grows increasingly difficult the
-- more fields you add to each data type or the more deeply nested your data
-- structures become.
--

makeLenses ''Atom
makeLenses ''Point

shiftAtomX' :: Atom -> Atom
shiftAtomX' = over (point . x) (+ 1)

{-

Here's how it would look like on GHCi:

*LensTutorial Control.Lens> let atom = Atom { _elemt = "hello world", _point = Point { _x = 0.0, _y = 0.0 }}

*LensTutorial Control.Lens> shiftAtomX atom
Atom {_elemt = "hello world", _point = Point {_x = 1.0, _y = 0.0}}

*LensTutorial Control.Lens> shiftAtomX' atom
Atom {_elemt = "hello world", _point = Point {_x = 1.0, _y = 0.0}}

-}

-- Example of deeply nested objects
--
data Molecule = Molecule { _atoms :: [Atom] } deriving Show

makeLenses ''Molecule

shiftMolecule :: Molecule -> Molecule
shiftMolecule = over (atoms . traverse . point . x) (+ 1)

