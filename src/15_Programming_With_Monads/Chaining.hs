
module Chaining where

import Control.Monad

data Datum = Datum {
  x :: Integer,
  y :: String,
  z :: Float
}

-- This snippet is constructed to understand a little more about generalized
-- lifting (via `liftM`) and chaining calls via `ap`

lookup1 key alist = case lookup key alist of
                        Just (Just s@(_:_)) -> Just s
                        _ -> Nothing

convert2Int someValue = case someValue of
                            Just v -> Just (read v :: Integer)
                            Nothing -> Nothing

convert2Float someValue = case someValue of
                              Just v -> Just (read v :: Float)
                              Nothing -> Nothing

-- This is function application (not an operator) which means its always
-- left-to-right evaluation. this should convince you 
chaining alist =
  Datum `liftM` (convert2Int $ lookup1 "x" alist)
           `ap` lookup1 "y" alist
           `ap` (convert2Float $ lookup1 "z" alist)

-- Is the above function `chaining` more pleasant to read or do you prefer the
-- following rendition? The key difference between these two renditions is that
-- `chaining2` is likely to appeal to haskell beginners since the evaluation is
-- innermost to outermost.
chaining2 alist = 
  ap (ap (liftM Datum (convert2Int $ lookup1 "x" alist)) (lookup1 "y" alist)) (convert2Float $ lookup1 "z" alist)

