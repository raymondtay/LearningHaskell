{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Lib
import Yesod

data Links = Links
-- data HelloWorld = HelloWorld

-- mkYesod "HelloWorld" [parseRoutes| / HomeR GET |]
mkYesod "Links" [parseRoutes|
 / HomeR GET
 /page1 Page1R GET
 /page2 Page2R GET
 |]

-- instance Yesod HelloWorld
instance Yesod Links

-- getHomeR :: Handler Html
-- getHomeR = defaultLayout [whamlet| Hello, World!|]
getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}> Go to page 1!|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go to page 1!|]

main :: IO ()
main = warp 3000 Links

