{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

import CovidData ( CovidData )
import StatReport
import HtmlReport ( htmlReport )
import Params ( cmdLineParser, Params(..) )

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

generateReports :: (Functor t, Foldable t) =>
                   Params -> t CovidData -> IO ()
generateReports Params {..} quotes = do
    putStrLn "Enter start and end date (including) for detailed stats"
    putStrLn "Start date (yyyy-mm-dd):"
    startDate <- getLine
    putStrLn "End date (yyyy-mm-dd):"
    endDate <- getLine
    putStrLn ""

    unless silent $ putStr textRpt
    unless hide $ putStr textUser
    saveHtml htmlFile htmlRpt
 where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'

    userStatInfo' = userStatInfo quotes "2020-01-01" "2020-02-02"
    textUser = userTextReport userStatInfo'

    htmlRpt = htmlReport title quotes statInfo'

    title =  "Historical Quotes for Covid"

    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html

readQuotes :: FilePath -> IO [CovidData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)