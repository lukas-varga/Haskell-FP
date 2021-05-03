module Main where

import Lib
import MyRandom.MyRandom
import System.Random (randomIO)
import Data.List

main :: IO ()
main = do
    seed <- randomIO :: IO Int
    let res = avg seed
    print res
