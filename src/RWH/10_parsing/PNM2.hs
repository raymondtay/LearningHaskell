
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char(isSpace)

data Greymap = Greymap {
    greyWidth :: Int ,
    greyHeight :: Int , 
    greyMax :: Int , 
    greyData :: L.ByteString } deriving (Eq)

instance Show Greymap where show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
            Nothing -> Nothing
            Just (num, rest)
                | num <= 0  -> Nothing
                | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

-- this will take a bytestring and if the parse succeeds, it will return a single 
-- parsed Greymap, along with the string that remains after parsing. That residual string will
-- be available for future parses.
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s = 
    matchHeader (L8.pack "P5") s >>?
    \s -> skipSpace((), s)       >>?
    (getNat . snd)               >>?
    skipSpace                    >>?
    \(width, s) -> getNat s      >>?
    skipSpace                    >>?
    \(height, s) -> getNat s     >>?
    \(maxGray, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

{-
The (>>?) function acts very simply: it takes a value as its left argument,
and a function as its right. If the value is not Nothing, it applies the function
to whatever is wrapped in the Just constructor. We have defined our function as an 
operator so that we can use it to chain functions together. Finally, we haven't provided
a fixity declaration for (>>?), so it defaults to infixl9 (left-associative, strongest operator 
precedence). Therefore the expression
a >>? b >>? c would be ((a >> ? b) >>? c)
-}

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ == Nothing
Just v  >>? f = f v

