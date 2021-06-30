{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Data.Foldable (traverse_)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H (
        Html,
        string,
        text,
        ToValue(toValue),
        (!),
        body,
        docTypeHtml,
        h1,
        head,
        i,
        img,
        style,
        title
    )
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Colonnade ( headed, Colonnade, Headed )
import Text.Blaze.Colonnade ( encodeHtmlTable )
import Fmt (pretty, Buildable)

import CovidData
    ( toDouble,
      lostVacc,
      CovidData(date, total_cases, new_cases, total_deaths, new_deaths,
                reproduction_rate, total_vaccinations, people_vaccinated,
                people_fully_vaccinated, new_vaccinations
--                , lost_vaccinations
                ))
import StatReport
    ( showCovid, 
      StatEntry(cfield, meanVal, minVal, maxVal, daysBetweenMinMax, highRoll, median))


viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty

colStats :: Colonnade Headed StatEntry Html                
colStats = mconcat
      [ headed "Quote Field" (i . string . show . cfield) 
      , headed "Mean" (viaFmt . meanVal)
      , headed "Min" (viaFmt . minVal)
      , headed "Max" (viaFmt . maxVal)
      , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
      , headed "7 days roll" (viaFmt . highRoll)
      , headed "Median" (viaFmt . median)
      ]

colData :: Colonnade Headed CovidData Html          
colData = mconcat
      [ headed "Day" (viaFmt . date)
      , headed "Total_cases" (viaFmt . showCovid . toDouble . total_cases)
      , headed "New_cases" (viaFmt . showCovid .  toDouble . new_cases)
      , headed "Total_deaths" (viaFmt . showCovid .  toDouble . total_deaths)
      , headed "New_deaths" (viaFmt . showCovid .  toDouble . new_deaths)
      , headed "Reproduction_rate" (viaFmt . showCovid .  toDouble . reproduction_rate)
      , headed "Total_vaccinations" (viaFmt . showCovid . toDouble . total_vaccinations)
      , headed "People_vaccinated" (viaFmt . showCovid .  toDouble . people_vaccinated)
      , headed "People_fully_vaccinated" (viaFmt . showCovid . toDouble . people_fully_vaccinated)
      , headed "New_vaccinations" (viaFmt . showCovid . toDouble . new_vaccinations)
      
      , headed "Lost_vaccinations" (viaFmt . showCovid . toDouble . lostVacc)
      ]

htmlReport :: (Functor t, Foldable t) =>
              String -> t CovidData -> [StatEntry] -> ByteString
htmlReport docTitle quotes statEntries = renderHtml $ docTypeHtml $ do
     H.head $ do
       title $ string docTitle
       style tableStyle
     body $ do
       h1 "Statistics Report for Covid"
       encodeHtmlTable mempty colStats statEntries    

       h1 "Covid Quotes Data for Covid"
       encodeHtmlTable mempty colData quotes        
  where
    tableStyle = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 5px}"
