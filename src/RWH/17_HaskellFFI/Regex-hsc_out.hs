{-# LINE 1 "Regex-hsc.hs" #-}

{-# LANGUAGE CPP, ForeignFunctionInterface #-}


-- Enable CPP, enable the FFI syntax, declare a module name
-- and then import some things from the base library.
--
module Regex where

import Foreign
import Foreign.C.Types



-- The following newtype is to represent PCRE compile-time options whose
-- representations is actually that of a CInt value.
newtype PCREOption = PCREOption { unPCREOption ::CInt } deriving (Eq, Show)

-- The variable listed after the "#const" refers to C-style constant 
-- defined in the "pcre.h" file; you WILL see it when you search for it in that
-- header file.
caseless :: PCREOption
caseless = PCREOption 1
{-# LINE 24 "Regex-hsc.hs" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 27 "Regex-hsc.hs" #-}

dotall :: PCREOption
dotall = PCREOption 4
{-# LINE 30 "Regex-hsc.hs" #-}
