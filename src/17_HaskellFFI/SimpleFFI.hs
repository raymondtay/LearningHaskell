{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

-- This defines a new Haskell function, c_sin, whose concrete implementation is in C, via
-- `sin` function. When `c_sin` is called, a call to the actual sin will be
-- made (using the standard C calling convention, indicated by `ccall`). The
-- haskell runtime passes control to C, which returns its result back to
-- Haskell. The result is then wrapped up as a Haskell value of type `CDouble`.
--
foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

-- now we wrap the C types back to Haskell's typesystem
fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))

main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]

