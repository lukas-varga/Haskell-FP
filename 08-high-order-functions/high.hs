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