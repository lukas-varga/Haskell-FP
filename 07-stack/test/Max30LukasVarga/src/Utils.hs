module Utils where

import Data.Text as T (Text, strip, length , pack, unpack)

max30Chars :: String -> Bool
max30Chars xs =     if (T.length . T.strip $ T.pack xs) <= 30  then True
                    else False