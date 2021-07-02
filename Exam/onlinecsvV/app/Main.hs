{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv(FromNamedRecord, FromField, parseField, parseNamedRecord, runParser, decodeByName, (.:), FromNamedRecord)
import qualified Data.Vector as V(forM_)
import GHC.Generics (Generic)

newtype DefaulToMinusOne = DTMO Int 
                    deriving (Show, Eq, Ord)

instance FromField DefaulToMinusOne where 
    parseField s = case runParser (parseField s) of
            Left err -> pure $ DTMO (-1)
            Right n -> pure $ DTMO n

toInt :: DefaulToMinusOne -> Int 
toInt (DTMO d) = d

data Student = Student
    {
    studentId :: String
    , name :: String
    , age :: DefaulToMinusOne
    , avg :: Double
    }

instance FromNamedRecord Student where
  parseNamedRecord r = Student <$> r .: "studentId" <*> r .: "Name" <*> r .: "age" <*> r .: "avg"

main :: IO ()
main = do
    csvData <- BL.readFile "students.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->  -- this loops through the [Person] so that we can print out each record.
            putStrLn $ "Student Id: " ++ studentId p ++ " Name:  " ++  name p ++ "  Age:  " ++ show (toInt (age p))  ++ "  Average Mark:" ++ show (avg p)