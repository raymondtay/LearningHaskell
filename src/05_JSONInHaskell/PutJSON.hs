module PutJSON where

import Data.List (intercalate)
import SimpleJSON2
import Numeric
import Data.Bits
import Data.Char

data Doc = Empty 
        | Char Char
        | Text String
        | Line 
        | Concat Doc Doc
        | Union Doc Doc
        deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text r  = Text r

double :: Double -> Doc
double n = text (show n)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y 

line :: Doc
line = Line

hcat :: [Doc] -> Doc
hcat xs = fold (<>) 

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber s)   = show s
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name, val) = string name
                            <> text ": "
                            <> renderJValue val
renderJValue (JArray jarr) = series '[' ']' renderJValue jarr

-- renderJValue (JObject o) = "{" ++ pairs o ++ "}"
--     where pairs [] = ""
--           pairs ps = intercalate ", " (map renderPair ps) 
--           renderPair (k,v) = show k ++ ": " ++ renderJValue v
-- renderJValue (JArray a) = "[" ++ values a ++ "]"
--     where values [] = ""
--           values xs = intercalate ", " (map renderJValue xs)


-- good haskell style involves separating pure code from code that performs I/O.
-- our renderJValue function has no interaction with the outside world, but we still
-- need to be able to print a JValue 

concat :: [[a]] -> [a]
concat = foldr (++) []

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of 
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
           <> text (replicate (4 - length h) 'O')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep x = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x 

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (x:xs) = (x <> p) : punctuate p xs


