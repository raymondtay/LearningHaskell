{-
  Haskell does not require that a value or function to be declared or defined
  in a source file before it's used. It's perfectly normal for a definition to 
  come after the first place it's used. 
  The haskell compiler doesn't care about ordering at this level. This grants us
  the flexibility to structure our code in the manner that makes more logical sense
  to us, rather than follow an order that makes the compiler writer's life easier.

  Haskell module writers often use this flexibility to put "more important code"
  earlier in a source file, relegating "plumbing" to later. This is exactly how we are
  presenting the globToRegex function and its helpers here.
-}

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

