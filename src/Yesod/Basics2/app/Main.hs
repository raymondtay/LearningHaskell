{-# LANGUAGE OverloadedStrings,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies,
             ExtendedDefaultRules #-}


module Main where

import Yesod
import Data.Aeson (encode, decode)
import JsonData

data HelloWorld = HelloWorld -- customary first experience isn't it ?

-- There is a convention between the route 'HomeR' and the callback function
-- 'getHomeR' such that it wishes to provide an intuitive approach to linking
-- a route with its corresponding callback function.
--
mkYesod "HelloWorld" [parseRoutes|
/                 HomeR GET
/getMeAnotherJson JJ2   GET
/getMeJson        JJ    GET
|]

instance Yesod HelloWorld

getJJ :: HandlerFor HelloWorld Value
getJJ = return $ object ["msg" .= "Hello World!"]

getJJ2 :: Handler Value
getJJ2 = returnJson $ Person { name = "Raymond", age = 45 }

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet| Hello, World |]

main :: IO ()
main = warp 4300 HelloWorld


