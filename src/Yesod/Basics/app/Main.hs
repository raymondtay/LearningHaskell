{-# LANGUAGE
    ExtendedDefaultRules,
    OverloadedStrings,
    QuasiQuotes,
    TemplateHaskell,
    TypeFamilies #-}

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
 /page3 Page3R GET
 /page4 Page4R GET
 /page5 Page5R GET
 |]

-- instance Yesod HelloWorld
instance Yesod Links

-- getHomeR :: Handler Html
-- getHomeR = defaultLayout [whamlet| Hello, World!|]
getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}> Go to page 1!|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go to page 1!|]
getPage3R = return $ object ["msg" .= "Hello,World"] -- rendering as JSON
getPage4R = returnJson $ Person { name = "Raymond", age = 45 }

getPage5R = defaultLayout [whamlet| #{renderPerson}|]

main :: IO ()
main = warp 3000 Links


