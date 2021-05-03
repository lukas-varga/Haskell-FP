module Split.LibSplit where

import Data.Char(toUpper)
import Data.List.Split(splitOn)

ourSplit :: String -> [String]
ourSplit xs = splitOn "A" (map toUpper xs)