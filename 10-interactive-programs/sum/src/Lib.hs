module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc =  do  
    putStrLn "Enter x:"
    xIn <- getLine
    let x = read xIn :: Int

    putStrLn "Enter y"
    yIn <- getLine
    let y = read yIn :: Int

    let res = x+y
    putStr "x+y = "
    putStrLn $ show res
