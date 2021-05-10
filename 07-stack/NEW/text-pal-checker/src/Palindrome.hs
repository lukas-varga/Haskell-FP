module Palindrome where

import Data.Text as T (Text, filter, pack, reverse, toLower, unpack)
import Data.Char as C (isSpace, isPunctuation)

--import qualified Data.Text as T
--import qualified Data.Char as C

isPalindrome :: String -> Bool
isPalindrome x   
    | a == (T.unpack . T.reverse $ T.pack a) = True
    | otherwise = False where
        a = clearText x

clearText :: String -> String
clearText x = T.unpack . T.filter isNotPunctuation . T.filter isNotSpace . T.toLower $ T.pack x

isNotSpace :: Char -> Bool
isNotSpace x =          if not $ C.isSpace x then True
                        else False 

isNotPunctuation :: Char -> Bool
isNotPunctuation x =    if not $ C.isPunctuation x then True
                        else False