module StatReport where

import Data.List (sortOn)
import Data.Time (diffDays)
import Data.List (tails)
import Debug.Trace
import QuoteData

avgNewCases :: [QuoteData] -> Double --the average (new) covid cases over the entire period
avgNewCases all = sum (map new_cases all) / fromIntegral (length all)

computeLowDate :: [QuoteData] -> QField  -> QuoteData
computeLowDate   all onfield  = lowNC where
                      get = field2fun onfield
                      sorted = sortOn get all
                      lowNC = head sorted

computeHighDate :: [QuoteData] -> QField  -> QuoteData
computeHighDate   all onfield  = highNC where
                      get = field2fun onfield
                      sorted = sortOn get all
                      highNC = last sorted

computeHighRollNC:: [QuoteData] -> Double
computeHighRollNC all = res where
                    xs = map new_cases all
                    res = maxRoll 7 xs

maxRoll :: Int -> [Double] -> Double
maxRoll p [a,b,c,d,e,f,g] = (a+b+c+d+e+f+g) / fromIntegral p
maxRoll p xs = max (sum (take p xs) / fromIntegral p) (maxRoll p $ tail xs) 


computeAvgRangeNewCases :: [QuoteData] -> String -> String
computeAvgRangeNC = undefined

computeAvgRangeNewCases :: [QuoteData] -> String -> String
computeLowRangeNC = undefined

computeAvgRangeNewCases :: [QuoteData] -> String -> String
computeHighRangeNC = undefined


computeAvgRangeNewCases :: [QuoteData] -> String -> String
computeLowVC = undefined

computeAvgRangeNewCases :: [QuoteData] -> String -> String
computeHighVC = undefined

{-
avgOpen :: [QuoteData] -> Double  -- average  opening values
avgOpen all =  sum (map open all)  / fromIntegral (length all)

avgLow :: [QuoteData] -> Double  -- average  low values
avgLow all = sum (map low all) / fromIntegral (length all)

computeMinMaxDays :: [QuoteData] -> QField  -> (QuoteData, QuoteData, Int)
computeMinMaxDays   all onfield  = (minQ, maxQ, daysBetweenMinMax) where
                      get = field2fun onfield
                      sorted = sortOn get all
                      minQ = head sorted
                      maxQ = last sorted
                      daysBetweenMinMax = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)
-}
