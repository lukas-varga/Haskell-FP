{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

import CovidData ( CovidData )
import StatReport ( statInfo, textReport )
import HtmlReport ( htmlReport )
import Params ( cmdLineParser, Params(..) )

main :: IO ()
main = cmdLineParser >>= work

readQuotes :: FilePath -> IO [CovidData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

generateReports :: (Functor t, Foldable t) =>
                   Params -> t CovidData -> IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr textRpt
  saveHtml htmlFile htmlRpt
 where
   statInfo' = statInfo quotes
   textRpt = textReport statInfo'
   htmlRpt = htmlReport title quotes statInfo'

   title =  "Historical Quotes for Covid"

   saveHtml Nothing _ = pure ()
   saveHtml (Just f) html = BL.writeFile f html