module Generator.Rand where
    
import System.Random

randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed) :: [Double]