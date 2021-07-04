-- Module for handling cmd parameters
module Params(Params (..), cmdLineParser) where

-- Necessary libraries
import Options.Applicative
    ( Parser,
      helper,
      (<**>),
      optional,
      fullDesc,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strArgument,
      strOption,
      switch,
      execParser )
import Data.Text (Text, strip)

-- Data type for parameters
data Params = Params {
        fname :: FilePath,
        htmlFile :: Maybe FilePath,
        stats :: Bool,
        dates :: Bool
}

-- Defining of user interface of parameters
mkParams :: Parser Params
mkParams =
  Params <$>
             strArgument
               (metavar "FILE" <>
                help "CSV-Filepath")
             <*> optional (strOption $
               long "html" <> metavar "FILE" <>
               help "generate HTML report")
             <*> switch
                (long "stats" <> short 's' <> help "don't print overall statistics")
             <*> switch
                (long "dates" <> short 'd' <> help "don't print stats between two dates")

-- Creating parameter parser with working settings
cmdLineParser :: IO Params
cmdLineParser = execParser options
  where
    options = info (mkParams <**> helper)
                (fullDesc <> progDesc "Covid data processing")