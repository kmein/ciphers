module Text.Cipher.Caesar where

import Data.Maybe (fromMaybe)
import Text.Cipher.Util (alphabetPos, charAtPos)

caesar :: Int -> String -> String
caesar n = map caesarChar
    where
      -- caesarChar n c =
      --     fromMaybe c $ (charAtPos . (+ n)) =<< alphabetPos c
      rotate n xs = zipWith const (drop n $ cycle xs) xs
      caesarChar c = fromMaybe c $ lookup c dict
      dict =
          zip ['a'..'z'] (rotate (n `mod` 26) ['a'..'z'])
          ++ zip ['A'..'Z'] (rotate (n `mod` 26) ['A'..'Z'])

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)

