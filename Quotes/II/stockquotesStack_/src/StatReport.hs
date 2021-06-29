{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
import Fmt      -- used for formating text
    ( Buildable(..),
      Builder,
      (+|),
      (+||),
      pretty,
      (|+),
      (||+),
      fixedF )          
import Colonnade ( ascii, headed )

import QuoteData ( field2fun, QField(Volume), QuoteData(day) )

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {     --this is used to format the calculated fields
    decimalPlaces :: Int,
    value :: Double
  }

data StatEntry = StatEntry {     -- where we store our stat results
    qfield :: QField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a   --this allow us to calculate our mean for any foldable type
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t) =>      --this has an extra parameter 'get' whuch allows us to indicate which field of QuoteData to compute on
                                                  -- we use this in statInfo to compute for all the fields of QuoteData
                     (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get quotes = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp quotes
    maxQ = maximumBy cmp quotes
    days = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound]        -- this says 'use all of the qField values from start to finish in that order 
                                                                    -- from 'data QField = Open | Close | High | Low | Volume' in QuoteData
  where  
    decimalPlacesByQField Volume = 0                                -- this is for the integral field - no fractional part
    decimalPlacesByQField _ = decimalPlacesFloating                 -- the other fields get 2 decimal places 

    qFieldStatInfo qfield =
      let
        get = field2fun qfield                                       --this brings in the field (open, close that we are working on)
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get quotes
        decPlaces = decimalPlacesByQField qfield
        meanVal = StatValue decimalPlacesFloating
                            (mean $ fmap get quotes)                -- extract a Foldable with one field
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}                                               -- build up  a list of StatEntry as the result, usinga RecordWildCards to fill the record



instance Buildable StatValue where                                    -- we use instance to define the use of StatValue in statInfo
  build sv = fixedF (decimalPlaces sv) (value sv)                     -- format doubles - 'fixedF' of from fmt package

instance Buildable StatEntry where                                    -- we use instance to define how StatEntry will look. This structure will be used for text and html..   
  build StatEntry {..} =
           ""+||qfield||+": "                                         -- this code requires RecordWildCards and OverloadedStrings
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String                                    -- we get a built StatEntry and format it into a String for text purposes
textReport = ascii colStats                                            -- ascii is from Colonnade 
  where
    colStats = mconcat
      [ headed "Quote Field" (show . qfield)                           -- headed is from Colonnade
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

showPrice :: Double -> Builder                                            -- we will use this for html formatting (auxiliary function)
showPrice = fixedF decimalPlacesFloating
