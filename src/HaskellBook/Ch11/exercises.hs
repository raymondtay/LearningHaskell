module Chapter11 where

import Data.Int

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

data BigSmall = Big Bool | Small Bool deriving (Eq,Show)


