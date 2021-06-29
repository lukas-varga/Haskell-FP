module CovidParameters where

import Options.Applicative
import Data.Text (Text, strip)

data Parameters = Parameters {
    csvfile :: FilePath
    html :: Maybe FilePath
    text :: Bool
}

readparams :: Parser Params
readparameters =
  Parameters <$>
             strArgument
               (metavar "FILE" <> help "CSV-Filepath")
             <*> optional (strOption $
               long "HTML" <> metavar "FILE" <> help "HTML-Filepath")
             <*> switch
               (long "TEXT" <> short 'T' <> help "Statistics?")


parser :: IO Params
parser = execParser options
  where
    options = info (readparams <**> helper)
                (fullDesc <> progDesc "Stock quotes data processing")