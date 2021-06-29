{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
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

import CovidData ( field2fun, CField, CovidData(date), toDouble)

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
    daysBetweenMinMax :: Int
}

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays get covid = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp covid
    maxQ = maximumBy cmp covid
    days = fromIntegral $ abs $ diffDays (date minQ) (date maxQ)

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
      in StatEntry {..}                                              


instance Buildable StatValue where                                    
  build sv = fixedF (decimalPlaces sv) (value sv)                 

instance Buildable StatEntry where                                    
  build StatEntry {..} =
           ""+||cfield||+": "                                         
             +|meanVal|+" (mean), "
             +|minVal|+" (min), "
             +|maxVal|+" (max), "
             +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String                                  
textReport = ascii colStats                                            
  where
    colStats = mconcat
      [ headed "Covid Field" (show . cfield)                        
      , headed "Mean" (pretty . meanVal)
      , headed "Min" (pretty . minVal)
      , headed "Max" (pretty . maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

showCovid :: Double -> Builder
showCovid = fixedF decimalPlacesFloating