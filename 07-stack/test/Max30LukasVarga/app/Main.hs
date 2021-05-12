module Main where

import Lib
import Utils

main :: IO ()
main = do
    input <- getLine
    let answer =    if max30Chars input == True then "this string is valid (i.e. not more than 30 characters)"
                    else "this string is invalid (i.e. more than 30 characters)"
    print answer
