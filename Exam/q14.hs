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