module Main where

import Lib
import Generator.Rand
import System.Random

main :: IO ()
main = do
    newRand <- randomIO :: IO Int
    let result = (randomList newRand) !! 1
    print result