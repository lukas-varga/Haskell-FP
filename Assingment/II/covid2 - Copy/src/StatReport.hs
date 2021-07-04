{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module for calculating stats
module StatReport where

-- Necessary libraries
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
import Numeric ( showFFloat )

-- Importing other modules
import CovidData ( field2fun, CField, CovidData(date), toDouble, DefaultToZero, makeDate, lostVacc, lostStated)

-- Setting default decimal place to 2
decimalPlacesFloating :: Int
decimalPlacesFloating = 2

-- Each entry can be represented by its value and number of decimal places
data StatValue = StatValue {
    decimalPlaces :: Int,
    value :: Double
}

-- Main type for storing stats before printing 
data StatEntry = StatEntry {
    cfield :: CField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int,
    highRoll :: StatValue,
    median :: StatValue,
    suma :: StatValue
}

-- Function computes mean/average of data
mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

-- Computes max, min and difference between those two
computeMinMaxDays :: (Ord a, Foldable t, Num c) => (CovidData -> a) -> t CovidData -> (a, a, c)
computeMinMaxDays get covid = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp covid
    maxQ = maximumBy cmp covid
    days = fromIntegral $ abs $ diffDays (date minQ) (date maxQ)

-- The highest rolling '7 day' number over the entire period
computeHighRoll :: (Foldable t) => t Double -> Double
computeHighRoll fol = maxRoll 7 (toList fol)

-- Auxiliary recursive function to rolling number
maxRoll :: Int -> [Double] -> Double
maxRoll p [a,b,c,d,e,f,g] = (a+b+c+d+e+f+g) / fromIntegral p
maxRoll p xs = max (sum (take p xs) / fromIntegral p) (maxRoll p $ tail xs)

-- Compute median value (middle value) in given field
-- If number of records is odd it returns middle value, else if it is even it return average of those two middle value
computeMedian :: (Foldable t) =>  t Double -> Double
computeMedian fol = res
  where
    unSorted = toList fol
    arr = sort unSorted
    twoAvg = (last (fst (halve arr)) + head (snd (halve arr))) / 2.0
    res =   if odd $ length arr then head $ snd $ halve arr
            else twoAvg

-- Auxiliary method for splitting array in half
halve :: [Double] -> ([Double], [Double])
halve xs = splitAt mid xs where
                 mid = length xs `div` 2

-- Given two dates it computes the average, highest and lowest number during that time
computeRange :: (Fractional a, Ord a, Foldable t) => (CovidData -> a) -> t CovidData -> String -> String -> (a, a, a, Int)
computeRange get covid startString endString  = (avg, low, high, daysBetween)
  where
    startDay = makeDate startString
    endDay = makeDate endString
    covidRec = sort (toList covid)
    select = [ x | x <- covidRec, date x >= startDay && date x <= endDay ]
    arr = fmap get select
    sorted = sort arr

    avg = sum sorted / fromIntegral (length sorted)
    low = head sorted
    high = last sorted
    daysBetween = fromIntegral $ abs $ diffDays startDay endDay

-- Computes sum of the column
computeSum :: (Fractional a, Foldable t) => t a -> a
computeSum = sum

-- Method use for formatting lost vaccine output
-- Source: https://stackoverflow.com/questions/37006362/how-to-get-a-decimal-string-from-a-double-instead-of-scientific-notation-in-hask
showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

-- Compute lost vaccinations from data from two fields
computeLost :: (Foldable t) => t CovidData -> String
computeLost covid = nicer
  where
    nicer = pretty $ show (showFullPrecision res)
    res = maximum arr
    arr = fmap (toDouble . lostVacc) $ toList covid

-- Displaying lost vaccinations from data
displayLost :: (Foldable t) => t CovidData -> String
displayLost covid = nicer 
  where
    nicer = pretty $ show (showFullPrecision res)
    res = maximum arr
    arr = fmap (toDouble . lostStated) $ toList covid

-- This method contains all the logic for computing overall stats
statInfo :: (Functor t, Foldable t) => t CovidData -> [StatEntry]
statInfo covid = fmap cFieldStatInfo [minBound .. maxBound]
  where
--    decimalPlacesByCField CFIELD = 0                         
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
        suma = StatValue decimalPlacesFloating
                              (computeSum $ fmap get covid)
      in StatEntry {..}

-- This method contains all the logic for computing stats between two dates
userStatInfo :: (Functor t, Foldable t) => t CovidData -> String -> String -> [StatEntry]
userStatInfo covid start end = fmap cFieldUserStatInfo [minBound .. maxBound]
  where
--    decimalPlacesByCField CFIELD = 0                         
    decimalPlacesByCField _ = decimalPlacesFloating

    cFieldUserStatInfo cfield =
      let
        get = field2fun cfield
        (avgUser, mn, mx, daysBetweenMinMax) =
              computeRange get covid start end
        decPlaces = decimalPlacesByCField cfield
        meanVal = StatValue decPlaces avgUser
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}

-- Thanks to this method our Entries are buildable and can be processed by collonade
instance Buildable StatEntry where
  build StatEntry {..} =
           ""+||cfield||+": "
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days), "
             +|highRoll|+" (roll), "
             +|median|+" (median), "
             +|suma|+" (sum)"

-- Handling decimals with Entries
instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

-- Preparing table with text outputs for overall stats
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
      , headed "Sum" (pretty . suma)
      ]

-- Preparing table with text outputs for stats between two dates
userTextReport :: [StatEntry] -> String
userTextReport = ascii colStats
  where
    colStats = mconcat
      [ headed "Covid Field" (show . cfield)
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

-- Auxiliary method for making decimal place from double value
showCovid :: Double -> Builder
showCovid = fixedF decimalPlacesFloating