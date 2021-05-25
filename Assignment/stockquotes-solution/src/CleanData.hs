module CleanData where

import Data.List.Split (splitOn)

-- Separate the flat string into an array of Strings taking out the heading
getImportedData :: String -> [[String]]
getImportedData csvRawData =  drop 1 $ filter (/=[""]) ( map (splitOn ",") $ splitOn "\n" csvRawData )

















