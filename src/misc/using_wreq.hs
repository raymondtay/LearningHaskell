module HttpStuff where

import Data.ByteString.Lazy
import Network.Wreq

urls :: [String]
urls = ["http://httpbin.org/ip",
        "http://httpbin.org/bytes/5"]

mappingGet:: [IO (Response ByteString)]
mappingGet = Prelude.map get urls

-- 
-- What if we don't want a list of IO actions we can perform to 
-- get a response, but rather one big IO action that produces a list 
-- of responses? This is where Traversable can be helpful.
--
traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls


