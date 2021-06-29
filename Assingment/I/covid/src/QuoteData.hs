module QuoteData where

import Data.Time(Day, parseTimeOrError,  defaultTimeLocale, diffDays)
import CleanData   

-- Constructor of data type QuoteData
data QuoteData = QuoteData {  
                    day :: Day,
                    total_cases :: Double,
                    new_cases :: Double,
                    total_deaths :: Double,
                    new_deaths :: Double,
                    total_vaccinations :: Double,
                    new_vaccinations :: Double
                }
    deriving (Ord, Eq, Show)

-- Auxiliary data types for QuoteData
data QField = Total_cases | New_cases | Total_deaths | New_deaths | Total_vaccinations | New_vaccinations
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Convert type into function
field2fun :: QField -> QuoteData -> Double
field2fun Total_cases = total_cases
field2fun New_cases = new_cases
field2fun Total_deaths = total_deaths
field2fun New_deaths = new_deaths
field2fun Total_vaccinations = total_vaccinations
field2fun New_vaccinations = new_vaccinations   

-- We use parseTimeError which takes the following defaults 
makeDate :: String -> Day
makeDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"  

-- Convert each of the strings to an appropriate type in form of tuple
strToTuple :: [String] -> (Day, Double, Double, Double, Double, Double, Double)
strToTuple (day:total_cases:new_cases:total_deaths:new_deaths:total_vaccinations:new_vaccinations:_)  = 
             (makeDate day, read total_cases:: Double, read new_cases:: Double, 
             read total_deaths ::Double,  read new_deaths :: Double, 
             read total_vaccinations :: Double, read new_vaccinations :: Double)

-- Convert tuple to QuoteData
tupleToQuote :: (Day, Double, Double, Double, Double, Double, Double)  -> QuoteData
tupleToQuote (day, total_cases, new_cases, total_deaths, new_deaths, total_vaccinations, new_vaccinations)  = QuoteData day total_cases new_cases total_deaths new_deaths total_vaccinations new_vaccinations

-- Make QuoteData from data
makeAllQuotes::String -> [QuoteData]
makeAllQuotes csvData = map (tupleToQuote . strToTuple ) (getImportedData csvData)
