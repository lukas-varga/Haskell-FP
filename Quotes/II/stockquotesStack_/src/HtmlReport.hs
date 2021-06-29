{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Data.Foldable (traverse_)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H
    ( Html,
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
      title )
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Colonnade ( headed, Colonnade, Headed )
import Text.Blaze.Colonnade ( encodeHtmlTable )
import Fmt (pretty, Buildable)

import QuoteData ( QuoteData(day, open, close, high, low, volume) )
import StatReport
    ( showPrice,
      StatEntry(qfield, meanVal, minVal, maxVal, daysBetweenMinMax) )

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty

colStats :: Colonnade Headed StatEntry Html                 -- the table of min, max
colStats = mconcat
      [ headed "Quote Field" (i . string . show . qfield)   -- the quote field is formatted as italic, string converys String to Html
      , headed "Mean" (viaFmt . meanVal)
      , headed "Min" (viaFmt . minVal)
      , headed "Max" (viaFmt . maxVal)
      , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
      ]

colData :: Colonnade Headed QuoteData Html                  -- the table of all data, by field
colData = mconcat
      [ headed "Day" (viaFmt . day)
      , headed "Open" (viaFmt . showPrice . open)
      , headed "Close" (viaFmt . showPrice . close)
      , headed "High" (viaFmt . showPrice . high)
      , headed "Low" (viaFmt . showPrice . low)
      , headed "Volume" (viaFmt . volume)
      ]

htmlReport :: (Functor t, Foldable t) =>
              String -> t QuoteData -> [StatEntry] -> [FilePath] -> ByteString
htmlReport docTitle quotes statEntries images = renderHtml $ docTypeHtml $ do
     H.head $ do
       title $ string docTitle
       style tableStyle
     body $ do
       unless (null images) $ do
         h1 "Charts"
         traverse_ ((img!).src.toValue) images

       h1 "Statistics Report for Stocks"
       encodeHtmlTable mempty colStats statEntries    -- encodeHtmlTable from Text.Blaze.Colonnade, using structure as definedin colStats

       h1 "Stock Quotes Data for Stocks"
       encodeHtmlTable mempty colData quotes          -- encodeHtmlTable from Text.Blaze.Colonnade, using structure as definedin colData
  where
    tableStyle = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 5px}"
