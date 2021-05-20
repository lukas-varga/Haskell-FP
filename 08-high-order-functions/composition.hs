--------------------EXERCISE 01--------------------
bigCubes :: [Int] -> [Int]
bigCubes xs = filter (\x -> x>500) $ map (^3) xs

--------------------EXERCISE 02--------------------
lottaBiggest :: [Int] -> [Int]
lottaBiggest xs = replicate 4 (maximum xs)

lottaBiggest' :: [Int] -> [Int]
lottaBiggest' xs = replicate 4 $ maximum xs

lottaBiggest'' :: [Int] -> [Int]
lottaBiggest'' xs = ((replicate 4) . maximum) xs
--lottaBiggest'' = ((replicate 4) . maximum)

--------------------EXERCISE 03--------------------
powers :: Int -> [Int]
powers x = [x^2] ++ [x^3] ++ [x^4]

powers' :: Int -> [Int]
powers' x = map ($x) [(^2),(^3),(^4)] 

--------------------EXERCISE 04--------------------
calcBill' :: [Float] -> [Float] ->[Float]
calcBill' amts pcts = map (*1.04) (zipWith (*) (map (+1)pcts) amts)

calcBill :: [Float] -> [Float] ->[Float]
calcBill amts pcts = map (*1.04) . zipWith (*) amts $ map (+1) pcts


