import qualified Data.ByteString.Lazy as L

-- We import the ByteString modules using Haskell's qualified import syntaqx
-- the import qualified that we just saw. This lets us refer to a module with a
-- name of our choosing.
-- 
-- For instance, when we want to refer to the lazy ByteString module's take function
-- we must write L.take since we imported the module u nder the name L. 
-- If we are not explicit about which version the compiler wiull report an error. 

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

-- The lazy and strict ByteString modules are intended for binary I/O. The haskell data
-- type for representing bytes is Word8, if we ned to refer to it by name, weimport it from 
-- the Data.Word module. 
-- The L.pack function takes a list of Word8 values, and packs them into a lazy
-- ByteString (The L.unpack performs the reverse). the elfMagic function
-- compares the first four bytes of a ByteString against a magic number.

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)

The L.readFile function is the lazy ByteString equivalent of readFile. IOt
