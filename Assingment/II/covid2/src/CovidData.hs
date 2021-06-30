{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack, ByteString)
import GHC.Generics (Generic)
import Data.Csv (runParser, FromField (..), parseNamedRecord ,FromNamedRecord ,(.:), NamedRecord)

newtype DefaultToZero = DTZ Double 
                    deriving (Show, Eq, Ord)

instance FromField DefaultToZero where 
    parseField s = case runParser (parseField s) of
            Left err -> pure $ DTZ 0
            Right n -> pure $ DTZ n

toDouble :: DefaultToZero -> Double 
toDouble (DTZ d) = d

data CovidData = CovidData {
                  date :: Day,
                  total_cases :: DefaultToZero,
                  new_cases :: DefaultToZero,
                  total_deaths :: DefaultToZero,
                  new_deaths :: DefaultToZero,
                  reproduction_rate :: DefaultToZero,
                  icu_patients :: DefaultToZero,
                  total_vaccinations :: DefaultToZero,
                  people_vaccinated :: DefaultToZero,
                  people_fully_vaccinated :: DefaultToZero,
                  new_vaccinations :: DefaultToZero
--                  ,lost_vaccinations :: DefaultToZero
                }
              deriving (Generic, Show, Ord, Eq)

instance FromNamedRecord CovidData where
    parseNamedRecord m = CovidData
        <$> m .: "date"
        <*> m .: "total_cases"
        <*> m .: "new_cases"
        <*> m .: "total_deaths"
        <*> m .: "new_deaths"
        <*> m .: "reproduction_rate"
        <*> m .: "icu_patients"
        <*> m .: "total_vaccinations"
        <*> m .: "people_vaccinated"
        <*> m .: "people_fully_vaccinated"
        <*> m .: "new_vaccinations"

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data CField = Total_cases | New_cases | Total_deaths | New_deaths
                | Reproduction_rate | Icu_patients
                | Total_vaccinations | People_vaccinated
                | People_fully_vaccinated | New_vaccinations
--                | Lost_vaccinations
    deriving (Eq, Ord, Show, Enum, Bounded)

field2fun :: CField -> CovidData -> Double 
field2fun Total_cases               = toDouble . total_cases
field2fun New_cases                 = toDouble . new_cases
field2fun Total_deaths              = toDouble . total_deaths
field2fun New_deaths                = toDouble . new_deaths
field2fun Reproduction_rate         = toDouble . reproduction_rate
field2fun Icu_patients              = toDouble . icu_patients
field2fun Total_vaccinations        = toDouble . total_vaccinations
field2fun People_vaccinated         = toDouble . people_vaccinated
field2fun People_fully_vaccinated   = toDouble . people_fully_vaccinated
field2fun New_vaccinations          = toDouble . new_vaccinations
--field2fun Lost_vaccinations         = toDouble . lost_vaccinations


lostVacc :: CovidData -> DefaultToZero
lostVacc a = DTZ (field2fun Total_vaccinations a - field2fun People_vaccinated a)

