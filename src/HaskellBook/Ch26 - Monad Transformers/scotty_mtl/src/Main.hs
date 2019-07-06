{-# LANGUAGE FlexibleContexts,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}


module Main where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text

import Network.HTTP.Types.Method

import Web.Scotty
import Web.Scotty.Internal.Types

-- Internal types of Scotty
-- newtype ScottyT e m a =
--   ScottyT { runS :: State (ScottyState e m) a } deriving (Functor, Applicative, Monad)
-- newtype ActionT e m a =
--   ActionT { runAM :: ExceptT (ActionError e)
--                              (ReaderT ActionEnv (StateT ScottyResponse m)) a }
--          deriving (Functor, Applicative, Monad)
-- 
-- type ScottyM = ScottyT Text IO
-- type ActionM = ActionT Text IO

main :: IO ()
main = do
  putStrLn "Starting server"
  scotty 3000 lRoutes

lRoutes :: ScottyM ()
lRoutes = do
  addroute GET "/hello" $ do text "hello world!"
  addroute GET "/hello2" $ do text "hello world2!"

