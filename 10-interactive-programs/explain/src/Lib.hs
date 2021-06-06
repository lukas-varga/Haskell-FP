module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
        putStrLn "n:"
        n <- getLine 
        let numN = read n :: Integer
        getInteger numN []

getInteger :: Integer -> [Integer] -> IO () 
getInteger 0 arr = putStr $ show (sumsNInts (toInteger $ length arr) arr) ++ " which sums nInts" 
getInteger n arr = do
            line <- getLine
            let x = read line :: Integer
            getInteger (n-1) (arr ++ [x])

sumsNInts :: Integer -> [Integer] -> Integer
sumsNInts 0 _ = 0
sumsNInts n (x:xs) = x + sumsNInts (n-1) xs