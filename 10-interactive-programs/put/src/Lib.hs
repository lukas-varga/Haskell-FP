module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    putStrLn "n:"
    nIn <- getLine
    let n = read nIn :: Integer

    putStrLn "str:"
    str <- getLine

    putNtimes n str

putNtimes :: Integer -> String -> IO()
putNtimes 1 _ = putStrLn ""
putNtimes n str = do   
            putStrLn str
            putNtimes (n-1) str 
