module Main where

import System.Environment(getArgs)
import Lib
import QuoteData
import StatReport

-- Main function load data from given CSV
-- Details about data cleaning -> CleanData.hs
main :: IO ()
main  = do
    (file:_) <- getArgs
    quotes <- readFile file
    
---------------------------------------- Statistics WITHOUT user input ----------------------------------------

    let avgNC = computeAvg (makeAllQuotes quotes) New_cases
    let lowNC = computeLow (makeAllQuotes quotes) New_cases
    let highNC = computeHigh (makeAllQuotes quotes) New_cases
    let highRollNC = computeHighRoll (makeAllQuotes quotes) New_cases
    let medianNC = computeMedian (makeAllQuotes quotes) New_cases
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Cases             |       Number      |       Date       "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Average               | " ++ show avgNC ++ " | " ++ "---"
    putStrLn $ "| Lowest Number         | " ++ show (new_cases lowNC) ++ "\t\t   " ++ " | " ++ show(day lowNC)
    putStrLn $ "| Highest Number        | " ++ show (new_cases highNC) ++ "\t   " ++ " | " ++ show(day highNC)
    putStrLn $ "| Rolling 7d Number     | " ++ show highRollNC ++ "\t   " ++ " | " ++ "---"
    putStrLn $ "| Median                | " ++ show medianNC ++ "\t   " ++ " | " ++ "---"

    putStrLn   ""
    putStrLn   ""
    putStrLn   ""

    let avgND = computeAvg (makeAllQuotes quotes) New_deaths
    let lowND = computeLow (makeAllQuotes quotes) New_deaths
    let highND = computeHigh (makeAllQuotes quotes) New_deaths
    let highRollND = computeHighRoll (makeAllQuotes quotes) New_deaths
    let medianND = computeMedian (makeAllQuotes quotes) New_deaths
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Deaths            |       Number       |       Date       "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Average               | " ++ show avgND ++ " | " ++ "---"
    putStrLn $ "| Lowest Number         | " ++ show (new_deaths lowND) ++ "\t\t    " ++ " | " ++ show(day lowND)
    putStrLn $ "| Highest Number        | " ++ show (new_deaths highND) ++ "\t    " ++ " | " ++ show(day highND)
    putStrLn $ "| Rolling 7d Number     | " ++ show highRollND ++ " " ++ " | " ++ "---"
    putStrLn $ "| Median                | " ++ show medianND ++ "\t\t    " ++ " | " ++ "---"

    putStrLn   ""
    putStrLn   ""
    putStrLn   ""


    let avgNV = computeAvg (makeAllQuotes quotes) New_vaccinations
    let lowNV = computeLow (makeAllQuotes quotes) New_vaccinations
    let highNV = computeHigh (makeAllQuotes quotes) New_vaccinations
    let highRollNV = computeHighRoll (makeAllQuotes quotes) New_vaccinations
    let medianNV = computeMedian (makeAllQuotes quotes) New_vaccinations
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Vaccinations      |       Number      |       Date       "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Average               | " ++ show avgNV ++ " | " ++ "---"
    putStrLn $ "| Lowest Number         | " ++ show (new_vaccinations lowNV) ++ "\t\t   " ++ " | " ++ show(day lowNV)
    putStrLn $ "| Highest Number        | " ++ show (new_vaccinations highNV) ++ "\t   " ++ " | " ++ show(day highNV)
    putStrLn $ "| Rolling 7d Number     | " ++ show highRollNV ++ "" ++ " | " ++ "---"
    putStrLn $ "| Median                | " ++ show medianNV ++ "\t\t   " ++ " | " ++ "---"

    putStrLn   ""
    putStrLn   ""
    putStrLn   ""

---------------------------------------- Statistics WITH user input ----------------------------------------

    putStrLn "Enter start and end date (including) to know the average, highest and lowest number of new covid cases during that time"
    putStrLn "Start date (yyyy-mm-dd):"
    startDateCases <- getLine
    putStrLn "End date (yyyy-mm-dd):"
    endDateCases <- getLine
    putStrLn ""

    let (avgRangeNC, lowRangeNC, highRangeNC, diffDaysNC) = computeRange (makeAllQuotes quotes) New_cases startDateCases endDateCases
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Entered Range         | " ++ startDateCases ++ " -> " ++ endDateCases ++ " (" ++ show diffDaysNC ++ " days)"
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Cases             |       Number      |       Date       "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Average               | " ++ show avgRangeNC ++ "\t" ++ " | " ++ "---"
    putStrLn $ "| Lowest Number         | " ++ show (new_cases lowRangeNC) ++ "\t\t" ++" | " ++ show (day lowRangeNC)
    putStrLn $ "| Highest Number        | " ++ show (new_cases highRangeNC) ++ "\t\t" ++ " | " ++ show (day highRangeNC)
    
    putStrLn   ""
    putStrLn   ""
    putStrLn   ""

    putStrLn "Enter start and end date (including) to know what are the highest and lowest number of vaccinations during that time"
    putStrLn "Start date (yyyy-mm-dd):"
    startDateVacc <- getLine
    putStrLn "End date (yyyy-mm-dd):"
    endDateVacc <- getLine
    putStrLn ""

    let (avgRangeNV, lowRangeNV, highRangeNV, diffDaysNV) = computeRange (makeAllQuotes quotes) New_vaccinations startDateVacc endDateVacc
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Entered Range         | " ++ startDateVacc ++ " -> " ++ endDateVacc ++ " (" ++ show diffDaysNV ++ " days)"
    putStrLn   "---------------------------------------------------------------"
    putStrLn   "| New Vaccinations      |       Number      |       Date       "
    putStrLn   "---------------------------------------------------------------"
    putStrLn $ "| Lowest Number         | " ++ show (new_vaccinations lowRangeNV) ++ "\t\t" ++" | " ++ show (day lowRangeNV)
    putStrLn $ "| Highest Number        | " ++ show (new_vaccinations highRangeNV) ++ "\t\t" ++ " | " ++ show (day highRangeNV)

    putStrLn   ""
    putStrLn   ""
    putStrLn   ""