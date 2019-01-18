
import System.Environment
import System.Directory
import System.FilePath
import Data.List hiding (find)
import Control.Concurrent.Async

-- Example run:
-- #> Main.find "geturls.hs" "/Users/raymondtay/LearningHaskell/"
-- #> Just "/Users/raymondtay/LearningHaskell/src/ConcurrentParallelHaskell/concurrency_constructs_in_haskell/geturls.hs"
-- OR
-- #> :set args "geturls.hs" "/Users/raymondtay/LearningHaskell/"
-- #> main
-- #> Just "/Users/raymondtay/LearningHaskell/src/ConcurrentParallelHaskell/concurrency_constructs_in_haskell/geturls.hs"
--
find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs -- want only real files, not present or parent directories
  if any (== s) fs' then return (Just (d </> s)) else loop fs'
    where
      loop [] = return Nothing
      loop (f:fs) = do
        let d' = d </> f
        isdir <- doesDirectoryExist d'
        if isdir then
                 do r <- find s d'
                    case r of
                        Just _ -> return r
                        Nothing -> loop fs
                  else loop fs

subfind :: String
        -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir then inner asyncs else withAsync (find s p) $ \a -> inner (a:asyncs)

-- To Build: ghc -O2 -eventlog -threaded -rtsopts ./findseq_to_par.hs
-- To Run: ./findseq_to_par {"run_par"|"run_seq"} "some file" "some directory" +RTS -s -N2
--
main :: IO ()
main = do
  [f, s, d] <- getArgs
  case f of
      "run_par" -> do print "Running Par"; r <- findpar s d; print r
      "run_seq" -> do print "Running Seq"; r <- find s d; print r

findpar :: String -> FilePath -> IO (Maybe FilePath)
findpar s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs' then return (Just (d </> s))
                    else do
                      let ps = map (d </>) fs'
                      foldr (subfind s) dowait ps []
                        where 
                          dowait as = loop (reverse as)
                          loop [] = return Nothing
                          loop (a:as) = do
                            r <- wait a
                            case r of
                                Nothing -> loop as
                                Just a -> return (Just a)

