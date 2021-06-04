module Main where

import System.Environment(getArgs)
import Lib
import QuoteData
import StatReport

main :: IO ()
main  = do
    (file:_) <- getArgs
    quotes <- readFile file

    putStrLn $ ""
    putStrLn "Enter start and end date to know the average, highest and lowest number of new covid cases during that time"
    putStrLn "Start (yyyy-mm-dd):"
    startDateCases <- getLine
    putStrLn "Start (yyyy-mm-dd):"
    endDateCases <- getLine

    putStrLn $ ""

    putStrLn "Enter start and end date to know what are the highest and lowest number of vaccinations during that time"
    putStrLn "Start (yyyy-mm-dd):"
    startDateVacc <- getLine
    putStrLn "Start (yyyy-mm-dd):"
    endDateVacc <- getLine
    putStrLn $ ""
    
    let avgNC = avgNewCases $ makeAllQuotes quotes
    let lowNC = computeLowDate (makeAllQuotes quotes) New_cases
    let highNC = computeHighDate (makeAllQuotes quotes) New_cases
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Cases (All Time)  |       Number      |       Date       "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Average               | " ++ show (avgNC) ++ " | " ++ "---"
    putStrLn $ "| Lowest Number         | " ++ show(new_cases lowNC) ++ "\t\t   " ++ " | " ++ show(day lowNC)
    putStrLn $ "| Highest Number        | " ++ show(new_cases highNC) ++ "\t   " ++ " | " ++ show(day highNC)

    putStrLn $ ""
    putStrLn   ""
    putStrLn $ ""

    --let highRollNC = computeHighRollNC (makeAllQuotes quotes) New_cases
    let highRollNC = computeHighRollNC $ makeAllQuotes quotes
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Cases Rolling 7d  |      Number                          "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Highest number        | " ++ show(highRollNC) 

    putStrLn $ ""
    putStrLn   ""
    putStrLn $ ""

    let avgRangeNC = 
    let lowRangeNC = 
    let highRangeNC =
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Entered Range         | " ++ startDateCases ++ " - " ++ endDateCases
    putStrLn   "| New Cases             |       Number      |     Days Diff    "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Average               | " 
    putStrLn $ "| Lowest Number         | "
    putStrLn $ "| Highest Number        | "
    
    putStrLn $ ""
    putStrLn   ""
    putStrLn $ ""

    let lowRangeVC = 
    let highRangeVC =
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Entered Range         | " ++ startDateVacc ++ " - " ++ endDateVacc
    putStrLn   "| Vaccinations          |       Number      |     Days Diff    "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Lowest Number         | "
    putStrLn $ "| Highest Number        | "
    
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