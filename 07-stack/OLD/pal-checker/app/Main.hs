module Main where

import Lib
import Palindrome

main :: IO ()
main = do
    input <- getLine
    let answer = decide input
    print answer

decide input
    | isPalindrome input == True = "It is a palindrome"
    | otherwise = "It is not a palindrome"