module Main where

import System.Environment(getArgs)
import CleanData
import StatReport
import QuoteData


main :: IO ()
main  = do
  (file:_) <- getArgs
  quotes <- readFile file

  let (minQ, maxQ, numDays) = computeMinMaxDays  (makeAllQuotes quotes) Open
  putStrLn "-------------------------------------------------------------------------------"
  putStrLn "Category   | Minimum       | Maximum     | Average            | Days Difference"
  putStrLn "-------------------------------------------------------------------------------"
 

  putStrLn $  "Open       |" ++ show(open minQ ) ++ "     |"  ++  show (open maxQ)   ++
           "   |"  ++ show (avgOpen $ makeAllQuotes quotes)  ++    "   |"  ++show numDays  
  -- try to run a similar analysis on the Closing Data (and other numeric fields)