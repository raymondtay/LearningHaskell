{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , RankNTypes #-}

-- The source of this tutorial is found here:
-- https://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html
--
module LensTutorial where

-- The simplest problem that the lens library solves is updating deeply nested
-- records. Suppose you had the following nested Haskell data types:
--
import Control.Lens hiding (element)

data Atom = Atom { _element :: String, _point :: Point } deriving Show
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

-- The intuition here is to realize a few things about Lenses. The first thing
-- to know is to realize what does the type signature is telling you i.e. `Lens
-- a b` means that `a` is the outer type whilst `b` is the nested type aka
-- "smaller" type. Once you have realized that, you can do the following:
-- For 90% of use cases, you just:
--   Create lenses (using `makeLens`, lens or plain-old `fmap`)
--   Compose them (using `(.)`)
--   Consume them (using `view`, `set`, and `over`)
--
setElement :: Lens' Atom String
setElement = lens _element (\atom newE -> atom { _element = newE })

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

data Pair a = Pair a a deriving (Functor, Foldable, Traversable)

