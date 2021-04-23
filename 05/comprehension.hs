import Data.Char

--------------------EXERCISE 01--------------------
--[1,2,3,4,5,6]
ex_1 = [x | x <- [1..6]]

--------------------EXERCISE 02--------------------
--[10,20,30,40,50,60]
ex_2 = [x*10 | x <- [1..6]]

--------------------EXERCISE 03--------------------
--[(1,1),(2,2),(3,3),(4,4)]
ex_3 = [(x,y) | x <- [1..4],y <- [1..4], x==y]

--------------------EXERCISE 04--------------------
--[(1,2),(2,3),(3,4),(4,5)]
ex_4 = [(x,y) | x <- [1..5], y <- [1..5], x+1 == y]

--------------------EXERCISE 05--------------------
--myConstFunc = [(1,1),(2,1),(3,1),(4,1),(5,1)]
myConstFunc = [(x,1) | x <- [1..5]]

--------------------EXERCISE 06--------------------
--squares = [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
squares = [(x,y) | x <- [1..10], y <- [1..10^2], y == x^2]

--------------------EXERCISE 07--------------------
f1 :: [(Int, Int)]
f1 = [(x, y) | x <-[1..3], y<- [4..5]]

f2 :: [(Int, Int)]
f2 = [(x, y) | y<- [4..5], x <-[1..3]]

f3 :: [(Int, Int)]
f3 = [(y, x) | x <-[1..3], y<- [4..5]]

--------------------EXERCISE 08--------------------
isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)
evenList = [2*n | n <- [2,4,7], isEven n, n>3]

--------------------EXERCISE 09--------------------
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [x*2 | x <- xs]

--------------------EXERCISE 10--------------------
capitalize :: String -> String
capitalize xs = [toUpper x | x <- xs] 

--------------------EXERCISE 11--------------------
sigma = sum [x^2 | x <- [1..100]]

--------------------EXERCISE 12--------------------
sigma' :: Int-> Int
sigma' n = sum [x^2 | x <- [1..n]]

--------------------EXERCISE 13--------------------
matches :: Integer -> [Integer] -> [Integer]
matches i xs= [x | x <- xs, i == x]

elem' :: Integer -> [Integer] -> Bool
elem' i xs  | matches i xs == [] = False
            | otherwise = True

--pattern matching
elem'' :: Integer -> [Integer] -> Bool
elem'' i (_:x:_) 
        | i == x = True
        | otherwise = False


--------------------EXERCISE 14--------------------
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--------------------EXERCISE 15--------------------
square :: Int -> [(Int, Int)]
square n = [xy | xy <- grid n n, fst xy /= snd xy]

--------------------EXERCISE 16--------------------
myReplicate :: Int -> a -> [a]
myReplicate n val = [val | x <- [1..n]]

--------------------EXERCISE 17--------------------
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--------------------EXERCISE 17--------------------
factors :: Int -> [Int]
factors n = 
    [x | x <- [1..n], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], x == sum (take (length (factors x)-1) (factors x))]


