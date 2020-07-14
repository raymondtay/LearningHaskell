module Prettify where

import SimpleJSON

data Doc = TobeDefined deriving Show

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- Combines a list of Doc values into one, possibly wrapping lines if
-- the output will not fit on a single line.
fsep :: [Doc] -> Doc
fsep xs = undefined
