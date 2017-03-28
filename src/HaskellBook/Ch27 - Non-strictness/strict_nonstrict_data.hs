{-# LANGUAGE Strict #-}

module StrictTest where

blah x = 1

main = print (blah undefined)

-- So, the Strict and StrictData pragmas are a means to avoiding noise when
-- everything or almost everything in a module is supposed to be strict. You
-- can use the tilde for irrefutable patterns to recover laziness on a case by
-- case basis.
--


