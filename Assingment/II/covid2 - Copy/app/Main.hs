{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- Main file to bring everything together
module Main where

-- Necessary libraries
import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)

-- Necessary imports of other modules
import CovidData ( CovidData )
import StatReport
    ( statInfo, textReport, userStatInfo, userTextReport, computeLost, displayLost )
import HtmlReport ( htmlReport )
import Params ( cmdLineParser, Params(..) )

-- Main function
main :: IO ()
main = cmdLineParser >>= work

-- Loading and parsing data from file
work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

-- Generating report and printing HTML 
generateReports :: (Functor t, Foldable t) => Params -> t CovidData -> IO ()
generateReports Params {..} quotes = do
    -- Saving HTML
    saveHtml htmlFile htmlRpt
    -- Optional printing of overall stats
    unless stats $ do
      putStrLn "Overall Statistics"
      putStr textRpt
      putStrLn $ "Lost Vaccinations: " ++ unused ++ " (Calculated)"
      putStrLn $ "Lost Vaccinations: " ++ stated ++  " (Stated)"
      putStrLn ""
    -- Optional printing of between dates stats
    unless dates $ do
      putStrLn "Enter start and end date (including) for detailed stats"
      putStrLn "Start date (yyyy-mm-dd):"
      start <- getLine 
      putStrLn "End date (yyyy-mm-dd):"
      end <- getLine
      putStrLn ""
      putStrLn $ "Stats between " ++ start ++ " and " ++ end
      putStr $ textUser start end
 where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'

    userStatInfo' s e = userStatInfo quotes s e
    textUser s e = userTextReport $ userStatInfo' s e

    unused = computeLost quotes 
    stated = displayLost quotes

    htmlRpt = htmlReport title quotes statInfo'
    title =  "Historical Quotes for Covid"

    -- Creating final html
    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html

-- Unused function to bring data to list
readQuotes :: FilePath -> IO [CovidData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)