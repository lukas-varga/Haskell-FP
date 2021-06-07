data MyMaybe a = MyNothing | MyJust a deriving Show


mapMay :: (a -> b) -> MyMaybe a -> MyMaybe b 
mapMay _ MyNothing = MyNothing
mapMay f (MyJust x)   = MyJust (f x) 

instance Functor MyMaybe where 
   -- fmap :: (a -> b) -> MyMaybe a -> MyMaybe b 
   fmap = mapMay


instance Applicative MyMaybe where
    pure                        = MyJust
    (MyJust f) <*> (MyJust x)   = MyJust (f x)
    _          <*> _            = MyNothing

-- own impl.
safeHead :: [a] -> MyMaybe a
safeHead []   = MyNothing
safeHead xs   = MyJust (head xs)

safeLast :: [a] -> MyMaybe a
safeLast []   = MyNothing
safeLast xs   = MyJust (last xs)

-- fmap
safeHeadAll1 :: Functor f => f [a] -> f (MyMaybe a)
safeHeadAll1 = fmap safeHead

safeLastAll1 :: Functor f => f [a] -> f (MyMaybe a)
safeLastAll1 = fmap safeLast

-- <$>
safeHeadAll2 :: Functor f => f[a] -> f (MyMaybe a)
safeHeadAll2 xs = safeHead <$> xs

safeLastAll2 :: Functor f => f [a] -> f (MyMaybe a)
safeLastAll2 xs = safeLast <$> xs


fromMaybe :: MyMaybe Char -> Char 
fromMaybe MyNothing = '-'
fromMaybe (MyJust ch ) = ch

fromMaybeAll :: Functor f => f (MyMaybe Char) -> f Char
fromMaybeAll xs = fromMaybe <$> xs