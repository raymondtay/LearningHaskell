
module GlobRegex(
globToRegex,
matchesGlob
) where

-- Haskell does not require that a value or a function be declared 
-- or defined in a source file before it's used. Its' perfectly normal for
-- a definition to come after the first place it is used. The Haskell compiler 
-- does not care about ordering at this level. This grants us the flexibility
-- to structure our code in the manner that makes most logical sense to us,
-- rather than follow an order that makes the compiler writer's life easiest.
--
-- Haskell model writers often use this flexibility to put " more important" 
-- code ealier in a source file, relegating "plumbing" to later.
import Text.Regex.Posix ((=~))
import System.Directory
import Control.Monad
import Control.Exception


globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*' : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?' : cs) = '.' : globToRegex' cs
globToRegex' ('[' : '!': c: cs) = "[^" ++ c : charClass cs
globToRegex' ('[' : c: cs) = '[' : c : charClass cs
globToRegex' ('[' :_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs


-- The escape function ensures that the regexp engine will not interpret
-- certain characters as pieces of regular expression syntax.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
      where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']' : cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pattern = name =~ globToRegex pattern


doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pattern = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle ((const (return [])) :: SomeException -> IO [String]) $ do
      names <- getDirectoryContents dirName'
      let names' = if isHidden pattern
                 then filter isHidden names
                 else filter (not . isHidden) names
      return (filter (`matchesGlob` pattern) names')

isHidden ('.':_) = True
isHidden _ = False


