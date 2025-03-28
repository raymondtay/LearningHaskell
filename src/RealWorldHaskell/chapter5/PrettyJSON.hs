module PrettyJSON (
  renderJValue
                  ) where

import           Data.Bits  (shiftR, (.&.))
import           Data.Char  (ord)
import           Data.List  (intercalate)
import           JSONClass  (JAry (..))
import           Numeric    (showHex)
import           Prelude    hiding ((<>))
import           Prettify   (Doc, char, compact, double, fsep, hcat, pretty,
                             punctuate, text, (<>))
import           SimpleJSON (JValue (..))
import           SimpleJSON

renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JNumber n) = double n
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JArray xs) = series '[' ']' renderJValue xs
renderJValue (JObject o) = series '{' '}' field o
  where field (name, val) = string name <> text ": " <> renderJValue val

putJValue :: JValue -> IO ()
putJValue v = putStrLn . show $ renderJValue v

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise -> char c
   where mustEscape _c = _c < ' ' || _c == '\x7f' || _c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])


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
  | otherwise = astral (d - 0x10000)
  where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

