{-# LANGUAGE InstanceSigs #-}

module Chapter26 where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- we only need to use return once because it's one big Monad
--
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int) 
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap 

-- A terminological point to keep in mind when reading about monad transformers
-- is that when Haskellers say "base monad" they usually mean what is
-- structurally outermost.
--
--
type MyType a = IO [Maybe a] -- base monad is IO
