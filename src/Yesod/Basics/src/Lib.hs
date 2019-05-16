{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lib
    ( someFunc     ,
      renderPerson
    ) where

import Text.Hamlet (shamlet, hamlet, HtmlUrl)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text)



data MyRoute = SpecialRoute

data Person = Person { name :: String, age :: Int }

person = Person { name = "Ray", age = 44 }

renderPerson = renderHtml [shamlet|
<p>Hello, my name is #{name person} and I am #{show $ age person}.
<p>
  let's do some funny stuff with my name :#
  <b>#{sort $ map toLower (name person)}
  <p>Oh, and in 5 years i will be #{show ((+) 5 (age person))} years old.
|]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

