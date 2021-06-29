module StatReport where

import QuoteData
import Data.List (sortOn)
import Data.Time (diffDays)

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


