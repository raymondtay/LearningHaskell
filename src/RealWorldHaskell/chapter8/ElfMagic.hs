import qualified Data.ByteString.Lazy as L

-- Look at the first four bytes in the file and seeing if they match a specific
-- sequence of bytes. A byte sequence that identifies a file's type is often known
-- as magic number.
hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

-- Determine the file is a ELF by reading 64KB at once.
isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)

