{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

import QuoteData ( QuoteData )
import Charts ( plotChart )
import StatReport ( statInfo, textReport )
import HtmlReport ( htmlReport )
import Params ( cmdLineParser, Params(..) )

generateReports :: (Functor t, Foldable t) =>
                   Params -> t QuoteData -> IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
 where
   statInfo' = statInfo quotes
   textRpt = textReport statInfo'
   htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

   chartFname = "chart.svg" 
   title =  "Historical Quotes for Stock data"

   saveHtml Nothing _ = pure ()
   saveHtml (Just f) html = BL.writeFile f html

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

main :: IO ()
main = cmdLineParser >>= work

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)
