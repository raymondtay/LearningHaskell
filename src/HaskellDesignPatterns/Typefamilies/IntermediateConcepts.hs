{-# LANGUAGE AllowAmbiguousTypes #-} -- GHC was complaining between LoggerMonad and LoggerMonad0
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}



import Control.Monad.State
import Control.Monad.Identity
import Data.Map
import Data.Time.Clock
--
-- Let's create a "logger" typeclass and try to generalize it to accommodate
-- polymorphic implementations
--

type Message = String -- type alias to give meaning to my message

-- This 1st version of the logger has 1 deficiency and that is we are unable to
-- add effects to it as there's no Monad
class Logger logger where
  prevMessages :: logger -> [Message] -- given a logger, retrieve all messages
  logString :: Message -> logger -> logger

-- This 2nd version of the logger allows us to correct the deficiency by
-- associating an effects Monad (i.e. IO) into the state monad. Most of the
-- times, it is ok but it is limited in the sense that we are bound to the IO
-- monad. Fortunately, there is a way to resolve this via "TypeFamilies"
--
class Logger2 logger where
  prevMessages2 :: logger -> [Message] -- given a logger, retrieve all messages
  logString2 :: Message -> logger -> StateT logger IO ()

u = undefined -- shortcut

newtype MessageWrapper = MessageWrapper [Message] deriving (Eq, Show)
instance Logger2 MessageWrapper where
  prevMessages2 (MessageWrapper msgs) = msgs
  logString2 msg (MessageWrapper msgs) = StateT (\l -> return ((), MessageWrapper (msg : msgs)))
-- Example of run
-- > runStateT (logString2 "hi" >> logString2 "there" $ MessageWrapper ["everyone"]) $ MessageWrapper ["init"]
--   ((),MessageWrapper ["there","everyone"])
-- > runStateT ( logString2 "hi" $ MessageWrapper ["there"]) $ MessageWrapper [""]
--   ((),MessageWrapper ["hi","there"])

class Logger3 logger where
  type LoggerMonad logger :: * -> *
  prevMessagesM :: logger -> [Message]
  logStringM :: Message -> (LoggerMonad logger) ()


newtype ListWrapper = ListWrapper [Message] deriving (Eq, Show)
instance Logger3 ListWrapper where
  type (LoggerMonad ListWrapper) = State ListWrapper
  prevMessagesM (ListWrapper msgs) = reverse msgs
  logStringM s = do
    (ListWrapper msgs) <- get
    put $ ListWrapper (s : msgs)


function :: p -> p -- nobody writes stuff like this
function input = input

runComputation input = do
  logStringM "Starting computation"
  let y = function input
  logStringM "Ending computation"
  return y

newtype StampedMessages = StampedMessages (Data.Map.Map UTCTime String)
instance Logger3 StampedMessages where
  type (LoggerMonad StampedMessages) = StateT StampedMessages IO
  prevMessagesM (StampedMessages msgs) = Data.Map.elems msgs
  logStringM s = do 
    (StampedMessages msgs) <- get
    currentTime <- lift getCurrentTime
    put $ StampedMessages (Data.Map.insert currentTime s msgs)

newtype ConsoleLogger = ConsoleLogger [String]
instance Logger3 ConsoleLogger where
  type (LoggerMonad ConsoleLogger) = StateT ConsoleLogger IO
  prevMessagesM (ConsoleLogger msgs) = reverse msgs
  logStringM s = do
    (ConsoleLogger msgs) <- get
    lift $ putStrLn s
    put $ ConsoleLogger (s : msgs)


--
-- Thinking out loud: Can i actually even abstract the message type ? ie.
-- allowing other data types as messages instead of just plain strings?
--
class SUPERLOGGER logger message where
  type LoggerS logger :: * -> *
  type MessageS message :: * -> *
  prevMessagesS :: logger -> [message]
  logStringS :: message -> (LoggerS logger) ()

newtype MessageType = MessageType String deriving (Eq, Show)
newtype ListMessageWrapper = ListMessageWrapper [MessageType] deriving (Eq, Show)

instance SUPERLOGGER ListMessageWrapper MessageType where
  type (LoggerS ListMessageWrapper) = State ListMessageWrapper
  type (MessageS MessageType) = Identity
  prevMessagesS (ListMessageWrapper msgs) = reverse msgs
  logStringS s = do
    (ListMessageWrapper msgs) <- get
    put $ ListMessageWrapper (s : msgs)


