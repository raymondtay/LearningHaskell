
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- the blessing and curse of the IO monad is that it is extremely powerful. If
-- we believe that careful use of types helps us to avoid programming mistakes,
-- then the IO monad should be a great source of unease. Because the IO monad
-- imposes no restrictions on what we can do, it leaves us vulnerable to all
-- kinds of accidents.
--
-- How can we tame its power? Let's say that we would like guarantee to
-- ourselves that a piece of code can read and write files on the local
-- filesyste, but it will not access the network. We cannot use the plain IO
-- monad, because it won't restrict us.
--
module HandleIO (
  HandleIO, Handle,
  IOMode(..), 
  runHandleIO, 
  openFile,
  hClose,
  hPutStrLn
  ) where

import Control.Monad.Trans
import System.Directory
import System.IO (Handle, IOMode(..))
import qualified System.IO

newtype HandleIO a = HandleIO { runHandleIO :: IO a } deriving (Monad, Applicative, Functor)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

-- we can now use our restricted HandleIO monad to perform I/O:
safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h

-- Designing for Unexpected Uses
--
-- There is one small but significant problem with our HandleIO monad: it does
-- not take into account the possibility that we might occasionally need an
-- escape hatch. If we define a monad such as this, it is likely that we will
-- occasionally need to perform an I/O action that isn't allowed for by the
-- design of our monad.
--
-- The Control.Monad.Trans module defines a standard escape hatch, the MonadIO
-- typeclass. This defines a single function,liftIO, which lets us embed an IO
-- action in another monad:
--


instance MonadIO HandleIO where
  liftIO = HandleIO

-- This function demonstrates the embedding of a IO action like `safeHello`
-- into `tidyHello`.
tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)



{-

Classes for Monad Transformers

A Monad Transformer makes new monad out of an existing monad, such that computations of the old monad may be embedded in the new one. To construct
a monad with a desired set of features, one typically starts with a base monad, such as Identity, [] or IO and applies a sequence of monad transformers. 

Most monad transformer modules include the special case of applying the transformer to Identity. For example, State s is an abbreviation for StateT s Identity. Each Monad transformer also comes with an operation runXXX to unwrap the transformer, exposing a computation of the inner moad.

-}

