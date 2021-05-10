module Main where

import Lib
import Palindrome

main :: IO ()
main = do
    print "Enter the text:"
    input <- getLine
    let answer =    if isPalindrome input == True then "It is a palindrome"
                    else "It is not a palindrome"
    print answer
