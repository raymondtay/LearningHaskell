module JSONClass (
    JValue(..)
) where

import SimpleJSON

type JSONError = String

class JSON a where
    toJValue :: a-> JValue
    fromJValue :: JValue -> Either JSONError a

