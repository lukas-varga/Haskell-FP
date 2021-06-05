module StatReport where

import Data.Time(Day, parseTimeOrError,  defaultTimeLocale, diffDays)
import Data.List (sortOn)
import Data.List (tails)
import Debug.Trace
import QuoteData

-- the average over the entire period
computeAvg :: [QuoteData] -> QField -> Double
computeAvg all onfield = avg where
                    get = field2fun onfield 
                    avg = sum (map get all) / fromIntegral (length all)
                                
-- the lowest number over the entire period
computeLow:: [QuoteData] -> QField  -> QuoteData 
computeLow   all onfield  = lowNC where
                      get = field2fun onfield
                      sorted = sortOn get all
                      lowNC = head sorted

-- the highest number over the entire period
computeHigh :: [QuoteData] -> QField  -> QuoteData
computeHigh   all onfield  = highNC where
                      get = field2fun onfield
                      sorted = sortOn get all
                      highNC = last sorted


-- the highest rolling '7 day' number over the entire period
computeHighRoll:: [QuoteData] -> QField -> Double
computeHighRoll all onfield = res where
                    get = field2fun onfield
                    xs = map get all
                    res = maxRoll 7 xs

-- auxiliary recursive function to rolling number
maxRoll :: Int -> [Double] -> Double
maxRoll p [a,b,c,d,e,f,g] = (a+b+c+d+e+f+g) / fromIntegral p
maxRoll p xs = max (sum (take p xs) / fromIntegral p) (maxRoll p $ tail xs) 


-- given two dates, the average, highest and lowest number during that time.
computeRange :: [QuoteData] -> QField -> String -> String -> (Double, QuoteData, QuoteData, Int)
computeRange all onfield startString endString  = (avgNC, lowQ, highQ, daysBetween) where
                    startDay = makeDate startString
                    endDay = makeDate endString
                    get = field2fun onfield
                    sorted = sortOn get all
                    select = [ x | x <- sorted, day x >= startDay && day x <= endDay ] 
                    avgNC = sum (map get select) / fromIntegral (length select)
                    lowQ = head select
                    highQ = last select
                    daysBetween = fromIntegral $ abs $ diffDays startDay endDay
