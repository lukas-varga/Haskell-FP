module Main where

import Lib
import Split.LibSplit

main :: IO ()
main = do
    input <- getLine
    let result = ourSplit input
    print result
