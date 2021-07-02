-- Q11
sumAll::Integer -> Integer -> Integer 
sumAll a b =    if a > b then 0
                else b + sumAll a (b-1)