{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CovidData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..), runParser)


newtype DefaultToZero = DTZ Double deriving (Show, Eq, Ord)

instance FromField DefaultToZero where
  parseField s = case runParser (parseField s) of
    Left err -> pure $ DTZ 0
    Right n  -> pure $ DTZ n

dtztodouble:: DefaultToZero -> Double
dtztodouble (DTZ d) = d


data CoronaData = CoronaData {
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
                 }
  deriving (Generic, FromNamedRecord)

data CoronaField = Date | Total_cases | New_cases | Total_deaths | New_deaths | Reproduction_rate | Icu_patients | Total_vaccinations | People_vaccinated | People_fully_vaccinated | New_vaccinations
  deriving (Eq, Ord, Show, Enum, Bounded)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

coronadetails :: CoronaField -> CoronaData -> Double
coronadetails Total_cases = dtztodouble.total_cases
coronadetails New_cases = dtztodouble.new_cases
coronadetails Total_deaths = dtztodouble.total_deaths
coronadetails New_deaths = dtztodouble.new_deaths
coronadetails Reproduction_rate = dtztodouble.reproduction_rate
coronadetails Icu_patients = dtztodouble.icu_patients
coronadetails Total_vaccinations = dtztodouble.total_vaccinations
coronadetails People_vaccinated = dtztodouble.people_vaccinated
coronadetails People_fully_vaccinated = dtztodouble.people_fully_vaccinated
coronadetails New_vaccinations = dtztodouble.new_vaccinations