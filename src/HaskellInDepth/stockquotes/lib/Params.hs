module Params (Params(..), cmdLineParser) where 

import Data.Semigroup ((<>))
import Options.Applicative

data Params = Params { fname :: FilePath, company :: String, prices :: Bool, volumes :: Bool, no_text :: Bool, html :: Bool }

mkParams :: Parser Params
mkParams =
  Params <$>
    strArgument (metavar "FILE" <> help "CSV file name")
    <*> strOption (long "company" <> short 'c' <> help "stock company's name" <> value "")
    <*> switch (long "prices" <> short 'p' <> help "create file with prices chart")
    <*> switch (long "volumes" <> short 'v' <> help "create file with volumes chart")
    <*> switch (long "no-text" <> short 'n' <> help "don't print statistics report")
    <*> switch (long "html" <> short 'n' <> help "create file with HTML report")


cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where opts = info (mkParams <**> helper) (fullDesc <> progDesc "Stock quotes data processing")
