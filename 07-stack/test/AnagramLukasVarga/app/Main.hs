module Main where

import Lib
import Anagram

main :: IO ()
main = do
    inputFirst <- getLine
    inputSecond <- getLine
    let answer =    if isAnagram inputFirst inputSecond == True then "They are anagrams!"
                    else "These are not anagrams!"
    print answer
