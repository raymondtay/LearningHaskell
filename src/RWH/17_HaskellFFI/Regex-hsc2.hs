{-# LANGUAGE CPP, ForeignFunctionInterface #-}


-- Enable CPP, enable the FFI syntax, declare a module name
-- and then import some things from the base library.
--
module Regex where

import Foreign
import Foreign.C.Types

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe   as S
import qualified Data.ByteString.Internal as S

#include <pcre.h>

-- The following newtype is to represent PCRE compile-time options whose
-- representations is actually that of a CInt value.
newtype PCREOption = PCREOption { unPCREOption ::CInt } deriving (Eq, Show)


-- "#enum" gives us 3 fields to work with. The first is the name of the type we
-- would like the C defines to be treated as. This lets us pick something other
-- than just CInt for the binding. We chose PCREOption's to construct.
--
-- The second field is an optional constructor to place infront of the symbols.
-- This is specifically for the case we want to construct "newtype" values, and
-- where much of the grant work is saved. The final part of the "#enum" syntax
-- is self-explanatory: it just defines Haskell names for constants to be
-- filled in via CPP.
--
#{enum PCREOption, PCREOption
  , caseless        = PCRE_CASELESS
  , dollar_endonly  = PCRE_DOLLAR_ENDONLY
  , dotall          = PCRE_DOTALL
  , dupnames        = PCRE_DUPNAMES
  , extended        = PCRE_EXTENDED
  , extra           = PCRE_EXTRA
  , firstline       = PCRE_FIRSTLINE
  , multiline       = PCRE_MULTILINE
  , newline_cr      = PCRE_NEWLINE_CR
  , newline_crlf    = PCRE_NEWLINE_CRLF
  , newline_lf      = PCRE_NEWLINE_LF
  , no_auto_capture = PCRE_NO_AUTO_CAPTURE
  , ungreedy        = PCRE_UNGREEDY
 
 }

{-- snippet pcre_compile --}
foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)
{-- /snippet pcre_compile --}

{-- snippet compileReal --}
compile :: S.ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  S.useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
{-- /snippet compileReal --}


-- | combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

