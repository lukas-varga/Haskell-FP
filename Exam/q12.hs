-- Q12
myreverse :: [a] -> [a]
myreverse xs = foldr (\x y -> y ++ [x]) [] xs