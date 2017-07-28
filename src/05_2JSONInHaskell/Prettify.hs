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
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

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

-- the usual suspects
concat :: [[a]] -> [a]
concat = foldr (++) []

-- Combines a list of Doc values into one, possibly wrapping lines if
-- the output will not fit on a single line.
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- `softline` inserts a newline if the curren tline
-- has become too wide, or a space otherwise.
softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

-- flatten replaces a Line with a space, turning two lines into one longer line
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten (x `Union` _ ) = flatten x
flatten line           = Char ' ' 
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds)= 
          case d of 
              Empty -> transform ds
              Char c -> c : transform ds
              Text s -> s ++ transform ds
              Line -> '\n' : transform ds
              a `Concat` b -> transform (a : b : ds)
              _ `Union` b -> transform (b:ds)

pretty width x = best 0 [x]
  where best col (d:ds) = 
            case d of
                Empty -> best col ds
                Char c -> c : best (col + 1) ds
                Text s -> s ++ best (col + length s) ds
                Line -> '\n' : best 0 ds
                a `Concat` b -> best col(a:b:ds)
                a `Union` b -> nicest col (best col (a:ds)) (best col (b:ds))
        best _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise = b
                       where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n' : _ ) = True
w `fits` (c:cs) = (w - 1) `fits` cs

