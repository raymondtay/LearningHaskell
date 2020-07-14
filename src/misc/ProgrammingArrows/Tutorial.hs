{-# LANGUAGE Arrows #-}

module Tutorial where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

{-
   Any haskell function can behave as an arrow, because there is an Arrow instance for the function type constructor (->).
   In this tutorial, i will build a more interesting arrow than this, with th eability to maintain state (Something that a plain Haskell
   function arrow cannot do). Arrows can produce all sorts of effects, including I/O, but we will just explore simple examples.

-}


-- A plain haskell function as an arrow has type `a -> b`. The circuit arrow
-- has two distinguishing features: (a) its wrap in a `newtype` declaration to
-- cleanly define an Arrow instance. (b) In order for the circuit to maintain
-- its own internal state, our arrow returns a replacement for itself along
-- with the normal `b` output value.
newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b , b) }

-- For the `Circuit` to become an Arrow, it is necessary to make it both an
-- instance of Category and Arrow.
instance Cat.Category Circuit where
  id = Circuit $ \a -> (Cat.id, a)
  (.) = dot
    where
      dot (Circuit cir2) (Circuit cir1) = Circuit $ \a ->
        let (cir1', b) = cir1 a
            (cir2', c) = cir2 b
        in (dot cir2' cir1', c)

instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a) -- 'arr' is the lift function; similar to 'pure' & 'return'
  first (Circuit cir) = Circuit $ \(b, d) ->
    let (cir', c) = cir b
    in (first cir', (c, d))

-- This is basically how we might "run" a Circuit
runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x:xs) =
  let (cir', x') = unCircuit cir x
  in x' : runCircuit cir' xs

