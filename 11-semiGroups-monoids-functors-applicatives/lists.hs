square :: Num a => a -> a
square x = x^2

-- using map as before
squareList1 :: Num a => [a] -> [a]
squareList1 xs = map square xs    -- can drop xs here (eta reduction)

-- using fmap functor
squareList2:: Num a => [a] -> [a]
squareList2 xs = fmap square xs   -- can drop xs here (eta reduction)

-- using <$> renaming for fmap (note now infix)
squareList3 :: Num a => [a] -> [a]
squareList3 xs = square <$> xs

------------------------- LISTS -------------------------

add1 :: Num a => a -> a
add1 x = x + 1

-- using map as before
incList1 :: Num a => [a] -> [a]
incList1 = map add1

-- using fmap functor
incList2:: Num a => [a] -> [a]
incList2 = fmap add1

-- using <$> renaming for fmap (note now infix)
incList3 :: Num a => [a] -> [a]
incList3 xs = add1 <$> xs

------------------------- MAYBE -------------------------

-- Mapping a Maybe
squareMaybe :: Num a => Maybe a -> Maybe a
squareMaybe x = square <$> x

-- alternative solution
squareAllMaybe :: Num a => [Maybe a] -> [Maybe a]
squareAllMaybe xs = [square <$> x | x <- xs]

-- map
squareAllMaybe1 :: Num a => [Maybe a] -> [Maybe a]
squareAllMaybe1 xs = map squareMaybe xs

-- fmap
squareAllMaybe2 :: Num a => [Maybe a] -> [Maybe a]
squareAllMaybe2 xs = fmap squareMaybe xs

-- <$>
squareAllMaybe3 :: Num a => [Maybe a] -> [Maybe a]
squareAllMaybe3 xs = squareMaybe <$> xs


type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show
m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Simon"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "(087)999"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2