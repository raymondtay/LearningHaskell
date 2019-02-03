{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- An out-call is a call from Haskell to a foreign language. At the present
-- time, the FFI supports only calls to C, so that is all we describe here. In
-- the following, we refer to threads in C (i.e. POSIX, or Win32) as "Os
-- threads" to distinguish them from the Haskell threads created with `forkIO`.
--
--
import System.IO
import Foreign
import Foreign.C.Types
foreign import ccall "read"
c_read ::  CInt     -- file descriptor
       -> Ptr Word8 -- buffer for data
       -> CSize     -- size of buffer
       -> CSSize    -- bytes read, or -1 on error


main :: IO ()
main = undefined


