module Main where

import System.Environment(getArgs)
import Lib
import QuoteData
import StatReport

main :: IO ()
main  = do
    (file:_) <- getArgs
    quotes <- readFile file
    putStrLn quotes
{-
    let (minQ, maxQ, numDays) = computeMinMaxDays  (makeAllQuotes quotes) Open
    let (minLow, maxLow, daysLow) = computeMinMaxDays (makeAllQuotes quotes) Low
    putStrLn "-------------------------------------------------------------------------------"
    putStrLn "Category   | Minimum       | Maximum     | Average            | Days Difference"
    putStrLn "-------------------------------------------------------------------------------"

    putStrLn $  "Open       |" ++ show(open minQ ) ++ "     |"  ++  show (open maxQ)    ++ "        |"  ++ show (avgOpen $ makeAllQuotes quotes)    ++ "   |"   ++show numDays  
    -- try to run a similar analysis on the Closing Data (and other numeric fields)
    putStrLn $  "Low        |" ++ show(low minLow) ++ "     |"  ++  show (low maxLow)   ++ "        |"  ++ show (avgLow $ makeAllQuotes quotes)     ++ "   |"   ++show daysLow
-}