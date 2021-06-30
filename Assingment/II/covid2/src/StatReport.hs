{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import Data.Ord ( comparing )
import Data.Foldable ( maximumBy, minimumBy, Foldable (toList) ) 
import Data.Time (diffDays, ut1ToLocalTime)
import Fmt (
        Buildable(..),
        Builder,
        (+|),
        (+||),
        pretty,
        (|+),
        (||+),
        fixedF
    )
import Colonnade ( ascii, headed )
import Data.List ( sort )

import CovidData ( field2fun, CField, CovidData(date), toDouble, DefaultToZero)

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {
    decimalPlaces :: Int,
    value :: Double
}

data StatEntry = StatEntry {
    cfield :: CField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int,
    highRoll :: StatValue,
    median :: StatValue
}

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t, Num c) => (CovidData -> a) -> t CovidData -> (a, a, c)
computeMinMaxDays get covid = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp covid
    maxQ = maximumBy cmp covid
    days = fromIntegral $ abs $ diffDays (date minQ) (date maxQ)

-- the highest rolling '7 day' number over the entire period
computeHighRoll :: (Foldable t) => t Double -> Double 
computeHighRoll fol = maxRoll 7 (toList fol)

-- auxiliary recursive function to rolling number
maxRoll :: Int -> [Double] -> Double
maxRoll p [a,b,c,d,e,f,g] = (a+b+c+d+e+f+g) / fromIntegral p
maxRoll p xs = max (sum (take p xs) / fromIntegral p) (maxRoll p $ tail xs)

-- compute median value (middle value) in given field. if number of records is odd it returns middle value, else if it is even it return average of those two middle value
computeMedian :: (Foldable t) =>  t Double -> Double  
computeMedian fol = res 
  where
    unSorted = toList fol
    arr = sort unSorted       
    twoAvg = (last (fst (halve arr)) + head (snd (halve arr))) / 2.0
    res =   if odd $ length arr then head $ snd $ halve arr
            else twoAvg
                    
-- auxiliary method for splitting array in half
halve :: [Double] -> ([Double], [Double]) 
halve xs = (take mid xs, drop mid xs) where
                 mid = length xs `div` 2

statInfo :: (Functor t, Foldable t) => t CovidData -> [StatEntry]
statInfo covid = fmap cFieldStatInfo [minBound .. maxBound]
  where
--    decimalPlacesByCField Volume = 0                         
    decimalPlacesByCField _ = decimalPlacesFloating

    cFieldStatInfo cfield =
      let
        get = field2fun cfield
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get covid
        decPlaces = decimalPlacesByCField cfield
        meanVal = StatValue decimalPlacesFloating
                            (mean $ fmap get covid)
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx

        highRoll = StatValue decimalPlacesFloating
                              (computeHighRoll $ fmap get covid)
        median = StatValue decimalPlacesFloating
                              (computeMedian $ fmap get covid)
      in StatEntry {..}


instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where
  build StatEntry {..} =
           ""+||cfield||+": "
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days), "
             +|highRoll|+" (roll), "
             +|median|+" (median)"

textReport :: [StatEntry] -> String
textReport = ascii colStats
  where
    colStats = mconcat
      [ headed "Covid Field" (show . cfield)
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      , headed "7 days roll" (pretty . highRoll)
      , headed "Median" (pretty . median)
      ]

showCovid :: Double -> Builder
showCovid = fixedF decimalPlacesFloating


{-

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

-- compute median value (middle value) in given field. if number of records is odd it returns middle value, else if it is even it return average of those two middle value
computeMedian :: [QuoteData] -> QField -> Double  
computeMedian all onfield = res where
                    get = field2fun onfield
                    sorted = sortOn get all
                    arr = map get sorted         
                    twoAvg = (last (fst (halve arr)) + head (snd (halve arr))) / 2.0
                    res =   if odd $ length arr then head $ snd $ halve arr
                            else twoAvg
                    
-- auxiliary method for splitting array in half
halve :: [Double] -> ([Double], [Double]) 
halve xs = (take mid xs, drop mid xs) where
                 mid = length xs `div` 2

-}