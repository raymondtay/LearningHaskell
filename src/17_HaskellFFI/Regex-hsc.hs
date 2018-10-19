
{-# LANGUAGE CPP, ForeignFunctionInterface #-}


-- Enable CPP, enable the FFI syntax, declare a module name
-- and then import some things from the base library.
--
module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>

-- The following newtype is to represent PCRE compile-time options whose
-- representations is actually that of a CInt value.
newtype PCREOption = PCREOption { unPCREOption ::CInt } deriving (Eq, Show)

-- The variable listed after the "#const" refers to C-style constant 
-- defined in the "pcre.h" file; you WILL see it when you search for it in that
-- header file.
caseless :: PCREOption
caseless = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall :: PCREOption
dotall = PCREOption #const PCRE_DOTALL
