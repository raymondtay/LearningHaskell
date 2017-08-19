
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- Enable CPP, enable the FFI syntax, declare a module name
-- and then import some things from the base library.
--
module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption ::CInt } deriving (Eq, Show)

