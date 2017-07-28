module Prettify where

import SimpleJSON

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

data Doc = Empty | Char Char | Text String | Line | Concat Doc Doc | Union Doc Doc deriving (Show, Eq)

empty :: Doc 
empty = Empty 

char :: Char -> Doc
char c = Char c

hcat :: [Doc] -> Doc
hcat xs = undefined

string :: String -> Doc
string str = undefined

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = Concat x y


-- Combines a list of Doc values into one, possibly wrapping lines if
-- the output will not fit on a single line.
fsep :: [Doc] -> Doc
fsep xs = undefined
