myMult :: (Int, Int) -> Int
myMult x = fst x * snd x

multPairsList ::[(Int,Int)] -> [Int]
multPairsList xs = [myMult x | x <- xs]

whatDoIDo x y = map (*1.1) $ zipWith (+) x $ zipWith (*) x  y

doesSomething  :: Integer -> Integer -> Integer
doesSomething x 1 = x
doesSomething x y = x + doesSomething x (y-1)

doIt :: Int -> a-> [a]
doIt 0 _ = []
doIt n x = x: doIt (n-1) x

-- Q11
sumAll::Integer -> Integer -> Integer 
sumAll a b =    if a > b then 0
                else b + sumAll a (b-1)

-- Q12
myreverse :: [a] -> [a]
myreverse xs = foldr (\x y -> y ++ [x]) [] xs

-- Q13
data Student = Student { sname:: String , courseLeader:: Maybe Lecturer} deriving Show

student1 = Student {sname = "student1", courseLeader = Just rob}
student2 = Student {sname = "potentialstudent1", courseLeader = Nothing}
student3 = Student {sname = "student3", courseLeader = Just john}

data Lecturer = Lecturer {lname :: String , roomNumber :: Maybe String} deriving Show
rob = Lecturer {lname = "Robert", roomNumber = Just "311"}
john = Lecturer { lname = "John", roomNumber = Nothing}

-- Part 1.
getLeaderRoom :: Student -> Maybe String
getLeaderRoom std = getRoom $ courseLeader std

getRoom :: Maybe Lecturer -> Maybe String
getRoom Nothing = Nothing
getRoom (Just lec) = roomNumber lec

-- Part 2.
getStudentLeader :: Student -> String
getStudentLeader std = getLecturer $ courseLeader std

getLecturer :: Maybe Lecturer -> String
getLecturer Nothing = ""
getLecturer (Just lec) = lname lec

-- Q14
data Expr = Val Int | Div Expr Expr | Mul Expr Expr | Max Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv n m = if m==0 then
                              Nothing
                         else Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                            Nothing -> Nothing
                            Just m -> safediv n m
eval (Mul x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                            Nothing -> Nothing
                            Just m -> Just(m*n)
eval (Max x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                            Nothing -> Nothing
                            Just m -> Just(max m n)