import qualified Data.ByteString.Lazy as L

-- We import the ByteString modules using Haskell's qualified import syntaqx
-- the import qualified that we just saw. This lets us refer to a module with a
-- name of our choosing.
-- 
-- For instance, when we want to refer to the lazy ByteString module's take function
-- we must write L.take since we imported the module u nder the name L. 
-- If we are not explicit about which version the compiler wiull report an error. 

hasElfMagic :: L.ByteString -> Bool
hasElfMafic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]


