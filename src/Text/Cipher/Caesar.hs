-- | This module supplies encryption and decryption functionality using the
-- famous Caesar cipher (see <https://en.wikipedia.org/wiki/Caesar_cipher the
-- Wikipedia page> for reference).
module Text.Cipher.Caesar where

import Data.Maybe (fromMaybe)
import Text.Cipher.Util (alphabetPos, charAtPos)

-- | Given an integer, shifts all characters that are part of the standard
-- Latin alphabet (A to Z, both upper and lower case) by that amount to the
-- right.
--
-- >>> caesar 1 "abc"
-- "bcd"
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

-- | The inverse of the 'caesar' function. Shifts by the given integer to the
-- left.
--
-- prop> uncaesar (caesar n str) = str
uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)

