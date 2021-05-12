module Anagram where

import Data.List (sort)

isAnagram :: String -> String -> Bool
isAnagram xs ys =   if (sort xs) == (sort ys) then True
                    else False