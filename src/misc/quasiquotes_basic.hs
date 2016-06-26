{-# LANGUAGE QuasiQuotes #-}

-- 
-- Without the above LANGUAGE directive, the following would
-- fail to compile.
-- Also, here's a nice debug method which i can use using "-ddump-splices":
-- *Quasimodo Text.Trifecta> :set -ddump-splices
-- *Quasimodo Text.Trifecta> :r
-- [1 of 1] Compiling Quasimodo        ( quasiquotes_basic.hs, interpreted )
-- quasiquotes_basic.hs:(12,12)-(17,2): Splicing expression
--     "\n\
--     \123\n\
--     \abc\n\
--     \456\n\
--     \def\n"
--     ======>
--     "\n\
--     \123\n\
--     \abc\n\
--     \456\n\
--     \def\n"
--     Ok, modules loaded: Quasimodo.
--
module Quasimodo where

import Text.RawString.QQ

eitherOr :: String 
eitherOr = [r|
123
abc
456
def
|]

