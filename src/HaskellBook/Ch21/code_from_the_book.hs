{-# LANGUAGE InstanceSigs #-}

module SomeCodeFromBook where

data Query = Query 
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- there's a decoder function that makes some object from String
--
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined 

-- There's a query, that runs against DB and returns array of strings
--
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- there's some additoinal context initializer, that also has IO side-effects
--
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
      (Left err) -> return $ Left $ err
      (Right res) -> do
        a <- makeIoOnlyObj res
        return $ Right a

