{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lib
    ( someFunc     ,
      renderPerson ,
      MyRoute (..)
    ) where

import Text.Hamlet (shamlet, hamlet, HtmlUrl)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text)
import Data.Monoid hiding ((<>))


data MyRoute = SpecialRoute | Home | Time | Stylesheet deriving (Eq, Show)

instance Monoid MyRoute where
  mempty = Home -- default route is always "Home".

instance Semigroup MyRoute where
  (<>) SpecialRoute SpecialRoute = SpecialRoute
  (<>) Home Home = Home
  (<>) Time Time = Time
  (<>) Stylesheet Stylesheet = Stylesheet
  (<>) _ _ = Home -- any other route is something we don't understand, we go back to "Home"

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

