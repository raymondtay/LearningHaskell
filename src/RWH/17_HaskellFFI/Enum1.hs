{-# LANGUAGE CPP #-}

-- The C preprocessor
-- 
-- The simplest task when setting out to write a new FFI binding from Haskell
-- to C is to bind constants defined in C headers to equivalent Haskell values.
-- For example
--
-- #define XX 0x000111
-- #define YY 0x9919191
--
-- To export these values to Haskell, we need to insert them into a Haskell
-- source file somehow. One obvious way to do this is by using the C
-- preprocessor to substitute definitions from C into the Haskell source, which
-- we then compile as a normal Haskell source file. Using the preprocessor, we
-- can even declare simple constants, via textual substitutions on the Haskell
-- source file.
--
#define N 16

-- To run, execute from commandline `runhaskell Enum1.hs`
main = print [ 1 .. N ]


