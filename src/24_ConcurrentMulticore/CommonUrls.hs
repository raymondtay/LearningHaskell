module Main where

import Control.Parallel.Strategies
import Control.Monad (forM_)
import Data.List (foldl', sortBy)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import Text.Regex
import System.Environment (getArgs)
import LineChunks (chunkedReadWith)
import MapReduce (mapReduce)


-- To install PCRE on mac:
-- # brew install pcre (this will bring in the header files, which is needed for 'cabal install')
-- # cabal install regex-pcre
--
countUrls :: [L.ByteString] -> M.Map S.ByteString Int
countUrls = mapReduce rpar (foldl' augment M.empty . L.lines) rpar (M.unionsWith (+))
  where augment map line =
          case matchRegex pattern (strict line) of
            Just (_:url:_) -> M.insertWith (+) url 1 map
            _              -> map
        strict  = S.unpack $ S.concat . L.toChunks
        pattern = mkRegex "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"


main :: IO ()
main = do
  undefined

