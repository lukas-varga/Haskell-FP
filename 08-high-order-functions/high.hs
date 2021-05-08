import Data.Char

--------------------EXERCISE 01--------------------
comp :: (Integral a) => (a -> b) -> (a -> Bool) -> [a] -> [b] 
comp f p xs = [f x | x <- xs, p x]

comp' :: (Integral a) => (a -> b) -> (a -> Bool) -> [a] -> [b]
comp' f p xs = map f $ filter p xs

--------------------EXERCISE 02--------------------
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&) . p) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr ((||) . p) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then [x] ++ xs else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = foldr (\x xs -> if p x then xs else [x] ++ xs) []

--------------------EXERCISE 03--------------------
map' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map' f = foldr (\x xs -> f x : xs) []

filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

filter'' :: Foldable t => (a -> Bool) -> t a -> [a]
filter'' p = foldr (\x xs -> doJob x xs) []
    where doJob x xs 
            | (p x) = x:xs 
            | otherwise = xs

--------------------EXERCISE 04--------------------
capitalises :: String -> String
capitalises = map toUpper

--------------------EXERCISE 05--------------------
squareall :: [Int] -> [Int]
squareall = map (^2)

--------------------EXERCISE 06--------------------
nestedreverse :: [String] -> [String]
nestedreverse (x:xs) = reverse $ map reverse (x:xs)

--------------------EXERCISE 07--------------------
atfront :: a -> [[a]] -> [[a]]
atfront a = map (a:)

--------------------EXERCISE 08--------------------
lengths :: [String] -> [Int]
lengths xss = map length xss

--------------------EXERCISE 09--------------------
sumsq :: Int -> Int
sumsq n = sum $ map (^2) [1..n]

--------------------EXERCISE 10--------------------
myFilter p = concat . map box where
    box x
        | p x = [x]
        | otherwise = []

--------------------EXERCISE 11--------------------
wvovel :: [Char] -> [Char]
wvovel xs = filter isNotVowel xs

isNotVowel :: Char -> Bool
isNotVowel c = not $ elem c "aeiouy"

--------------------EXERCISE 12--------------------
wiv :: [String] -> [String]
wiv xs = map wvovel xs