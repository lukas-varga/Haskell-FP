module Params(Params (..), cmdLineParser) where

import Options.Applicative
import Data.Text (Text, strip)

data Params = Params {
        fname :: FilePath,
        htmlFile :: Maybe FilePath,
        silent :: Bool,
        median :: Bool
}

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
                (long "silent" <> short 's' <> help "don't print statistics")
             <*> switch
                (long "median" <> short 'm' <> help "print median")

cmdLineParser :: IO Params
cmdLineParser = execParser options
  where
    options = info (mkParams <**> helper)
                (fullDesc <> progDesc "Covid data processing")