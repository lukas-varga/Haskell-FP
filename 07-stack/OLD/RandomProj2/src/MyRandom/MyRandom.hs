module MyRandom.MyRandom where
    
import System.Random

randomList :: Int -> [Double]
randomList seed = take 10 (randoms (mkStdGen seed)) :: [Double]

avg :: Int -> Double
avg seed = (sum list) / (fromIntegral $ length list) where
    list = randomList seed

