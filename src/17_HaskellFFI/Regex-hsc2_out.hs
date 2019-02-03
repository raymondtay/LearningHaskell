{-# LINE 1 "Regex-hsc2.hs" #-}
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
caseless         :: PCREOption
caseless         = PCREOption 1
dollar_endonly   :: PCREOption
dollar_endonly   = PCREOption 32
dotall           :: PCREOption
dotall           = PCREOption 4
dupnames         :: PCREOption
dupnames         = PCREOption 524288
extended         :: PCREOption
extended         = PCREOption 8
extra            :: PCREOption
extra            = PCREOption 64
firstline        :: PCREOption
firstline        = PCREOption 262144
multiline        :: PCREOption
multiline        = PCREOption 2
newline_cr       :: PCREOption
newline_cr       = PCREOption 1048576
newline_crlf     :: PCREOption
newline_crlf     = PCREOption 3145728
newline_lf       :: PCREOption
newline_lf       = PCREOption 2097152
no_auto_capture  :: PCREOption
no_auto_capture  = PCREOption 4096
ungreedy         :: PCREOption
ungreedy         = PCREOption 512

{-# LINE 49 "Regex-hsc2.hs" #-}

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

