{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Text.Hamlet (HtmlUrl, hamlet)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"
render Time _ = "/time"
render Stylesheet _ = "/style.css"

template :: Text -> HtmlUrl MyRoute
template title = [hamlet|
  $doctype 5
  <html>
    <head>
      <title>#{title}
      <link re=stylesheet href=@{Stylesheet}>
        <body>
          <h1>#{title}
          |]

main :: IO ()
main = putStrLn $ renderHtml $ template "My Title" render

