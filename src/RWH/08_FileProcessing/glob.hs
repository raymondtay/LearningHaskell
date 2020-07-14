
-- Emulate the UNIX system tool, glob.
module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle)
import Control.Monad (forM)
import GlobReg (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMathing pat 
    | not(isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
    | otherwise = do
    case splitFileName pat of 
        ("", baseName) -> do 
            curDir <- getCurrentDirectory
            listMatches curDir baseName
        (dir, baseName) -> do
            dirs <- if isPattern dir then namesMatching (dropTrailingPathSeparator dir) else return [dir]
            let listDir = if isPattern baseName then listMatches else listPlain
            pathNames <- forM dirs $ \dir -> do -- `forM` maps the second argument (an IO action) over its first (a list) and returns the result
                baseNames <- listDir dir baseName
                return (map (dir </>) baseNames)
        return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists then return True else doesDirectoryExist name 

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName then getCurrentDirectory else return dirName
    handle (const (return [])) $ do names <- getDirectoryContents dirName'
        let names' = if isHidden pat then filter isHidden names else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False

-- listPlain returns either an empty or singleton list, depending on whether the single name it's passed exists
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName then doesDirectoryExist dirName else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])
