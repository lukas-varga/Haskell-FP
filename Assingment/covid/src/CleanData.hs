module CleanData where

import Data.List.Split (splitOn)

-- I dropped first three columns which were just the same for every record
-- Then I have dropped columns which I will not use in my statistics
-- So now I only have column date and columns for cases,deaths and vacinations (each with total_ and new_ variant)
-- I dropped last 3 records since they were missing some values but logically there should not have been empty and therefore they could distort output
-- Lastly, I changed all empty "" values to 0.0 so data are easily read
-- I found one negative value in column "new_deaths" but I assume it will not break the program

getImportedData :: String -> [[String]]
getImportedData csvRawData = drop 1 $ filter (/=[""]) (map (splitOn ",") $ splitOn "\n" csvRawData)