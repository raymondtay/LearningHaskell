module GlobReg (
    globToRegex
    , matchesGlob
) where

import Text.Regex.Posix((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*' : cs)          = ".*" ++ globToRegex' cs
globToRegex' ('?' : cs)          = "." ++ globToRegex' cs
globToRegex' ('[': '!' : c : cs) = "[^" ++ c : charClass cs
globToRegex' ('[': c : cs)       = '['   : c : charClass cs
globToRegex' ('[':_)             = error "unterminated character class"
globToRegex' (c:cs)              = escape c ++ globToRegex' cs

-- escape ensures that the regexp engine will not interpret certain characters
-- as piecs of regular expression syntax
escape :: Char -> String
escape c | elem c regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

-- charClass checks that a character class is correctly terminated.
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"


-- here's a couple of examples
-- *GlobReg Data.ByteString.Char8> "foo.c" =~ globToRegex "f??.c" :: Bool
-- True
-- *GlobReg Data.ByteString.Char8> "test.c" =~ globToRegex "t[ea]s*" :: Bool
-- True
-- 

matchesGlob :: FilePath -> String -> Bool
matchesGlob name pat = name =~ globToRegex pat

