module LineChunks (
  chunkedReadWith
  ) where

import Control.Exception (bracket, finally)
import Control.Monad (forM, liftM)
import Control.Parallel.Strategies (NFData)
import Control.DeepSeq
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Conc (numCapabilities)
import System.IO

data ChunkSpec = CS {
chunkOffset :: !Int64,
chunkLength :: !Int64
                    } deriving (Eq, Show)

withChunks :: (NFData a) =>
  (FilePath -> IO [ChunkSpec])
  -> ([LB.ByteString] -> a)
  -> FilePath
  -> IO a
withChunks chunkF process path = do
  (chunks, handles) <- chunkedRead chunkF path
  let r = process chunks
  (rnf r `seq` return r) `finally` mapM_ hClose handles

chunkedReadWith :: (NFData a ) => ([LB.ByteString] -> a) -> FilePath -> IO a
chunkedReadWith f path = withChunks (lineChunks (numCapabilities * 4)) f path

chunkedRead :: (FilePath -> IO [ChunkSpec])
            -> FilePath
            -> IO ([LB.ByteString], [Handle])
chunkedRead chunkF path = do
  chunks <- chunkF path
  liftM unzip . forM chunks $ \spec -> do
    h <- openFile path ReadMode
    hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
    chunk <- LB.take (chunkLength spec) `liftM` LB.hGetContents h
    return (chunk, h)
 

-- Since a server logfile is line oriented, we need an efficient way to break a
-- file into large chunks, while making sure that each chunk ends on a line
-- boundary. since a chunk might be tens of megabytes in size we don't want to
-- scan all of the data in a chunk to determine where its final boundary should
-- be. the approach works whether we choose a fixed chunk size or a fixed
-- number of chunks. Here, we opt for the latter.
--

lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
  bracket (openFile path ReadMode) hClose $ \h -> do
    totalSize <- fromIntegral `liftM` hFileSize h -- find the total size in bytes of the targeted file
    let chunkSize = totalSize `div` fromIntegral numChunks
        findChunks offset = do
          let newOffset = offset + chunkSize -- create windows of length = chunkSize
          hSeek h AbsoluteSeek (fromIntegral newOffset)
          let findNewline off = do -- as before, defining a function
                eof <- hIsEOF h
                if eof
                    then return [CS offset (totalSize - offset)]
                    else do
                      bytes <- LB.hGet h 4096
                      case LB.elemIndex '\n' bytes of -- remember its line-oriented so '\n'
                          Just n -> do
                            chunks@(c:_) <- findChunks (off + n + 1)
                            let coff = chunkOffset c
                            return (CS offset (coff - offset): chunks)
                          Nothing -> findNewline (off + LB.length bytes)
          findNewline newOffset
    findChunks 0

