import qualified Data.ByteString.Lazy.Char8 as L

closing = readPrice . (!!4) . L.split ','

-- since the function is written in point free style, we read from right to left.
-- The L.split function splits a lazy ByteString into a list of them, every time 
-- it finds a matching character. The (!!) operator retrieves the k-th element of a 
-- list.

readPrice :: L.ByteString -> Maybe Int
readPrice str = 
    case L.readInt str of 
        Nothing              -> Nothing
        Just (dollars, rest) ->
            case L.readInt (L.tail rest) of 
                Nothing            -> Nothing
                Just (cents, more) ->
                    Just (dollars * 100 + cents)
-- L.readInt parses an integer and returns both the integer and the remainder 
-- of the string once a run of digits is consumed. Our defn is slightly complicated
-- by L.readInt returning Nothing if parsing fails.

 

