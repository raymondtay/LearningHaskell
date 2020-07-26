{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}


import           Data.IORef
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)

app :: IORef Int -> IO Application
app init = do
  c <- readIORef init
  modifyIORef init (+1)
  return (_app init)

_app :: IORef Int -> Application
_app c req respond = do
  _c <- readIORef c
  putStrLn $ "Running: " ++ show _c
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello"

main :: IO ()
main = do
  init <- newIORef 0
  putStrLn $ "http://localhost:8080/"
  _app <- app init
  run 8080 _app


