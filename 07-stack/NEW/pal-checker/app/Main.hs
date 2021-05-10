module Main where

import Lib
import Palindrome

main :: IO ()
main = do
    print "Please enter text:"
    input <- getLine
    let answer =    if (isPalindrome input) then "It is a palindrome"
                    else "It is not a palindrome"
    print answer
