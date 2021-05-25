module CleanData where

import Data.List.Split (splitOn)

getImportedData :: String -> [[String]]
getImportedData csvRawData = drop 1 $ filter (/=[""]) (map (splitOn ",") $ splitOn "\n" csvRawData)