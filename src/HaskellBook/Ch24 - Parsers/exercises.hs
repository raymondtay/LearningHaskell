module Chapter_24 where

import Text.Trifecta

-- The type of `unexpected` is Parsing m => String -> m a
--
stop :: Parser a
stop = unexpected "stop"

