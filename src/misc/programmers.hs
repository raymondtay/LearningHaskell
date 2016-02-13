
module Programmers where

data OperatingSystem = 
  GnuPlusLinux
  | OpenBSD
  | Mac 
  | Windows deriving (Eq, Show)

data ProgrammingLanguage = 
  Haskell 
  | Agda
  | Idris
  | Java
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage } deriving (Eq, Show)

-- Turns out we can actually create a partial record, so as to speak
-- and the runtime only sends out a warning, at best.
{-
*Programmers> let partialAf = Programmer { os = Mac }

<interactive>:468:17: Warning:
    Fields of ‘Programmer’ not initialised: lang
    In the expression: Programmer {os = Mac}
    In an equation for ‘partialAf’: partialAf = Programmer {os = Mac}
*Programmers>

-}
