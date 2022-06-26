module OurExceptions where

import           Control.Exception

-- data NotDivThree = NotDivThree Int deriving (Show, Eq)
-- instance Exception NotDivThree

-- data NotEven = NotEven Int deriving (Eq, Show)
-- instance Exception NotEven

-- Sum Type to express more then 1 exception type, provided that they are
-- semantically similar
data EATD = NotDivThree Int | NotEven Int deriving (Eq, Show)
instance Exception EATD


evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i = throwIO (NotEven i)
  | otherwise = return i

type EA e = IO (Either e Int)
result1 =
  try (evenAndThreeDiv 0) :: EA EATD
result2 =
  try (evenAndThreeDiv 1) :: EA EATD
result3 =
  try (evenAndThreeDiv 6) :: EA EATD

