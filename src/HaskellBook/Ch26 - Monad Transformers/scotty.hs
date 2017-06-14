
{-# LANGUAGE OverloadedStrings #-}

-- Version 1
--
-- module Scotty where
-- 
-- import Control.Monad.Trans.Class
-- 
-- import Web.Scotty
-- 
-- import Data.Monoid(mconcat)
-- 
-- main = scotty 3000 $ do
--   get "/:word" $ do
--     beam <- param "word"
--     lift(putStrLn "hello")
--     html $ mconcat ["<h1>Scotty, ", beam , " me up!</h1>"]


module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Class
import Data.Monoid(mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT . lift . lift . lift)(putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
