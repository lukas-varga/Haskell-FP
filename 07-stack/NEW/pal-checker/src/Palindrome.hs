module Palindrome where

isPalindrome :: String -> Bool
isPalindrome text
    | text == reverse text = True
    | otherwise = False