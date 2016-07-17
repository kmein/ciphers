-- | This module, as the name suggests, serves as a utility module for ciphers
-- like the Vigenère cipher, which depends upon shifting alphabet characters by
-- some amount.
module Text.Cipher.Util where

import Data.Char (toLower, toUpper)
import Data.List (elemIndex, nub)

-- | Checks whether its argument is a member of the German vowel set (a, ä, e,
-- i, o, ö, u, ü).
isVowel :: Char -> Bool
isVowel = flip elem "aeiouäöü"

-- | Returns a character's position (0-based index) in the alphabet.
--
-- __Note:__ `c' can be passed as both lower and upper case.
alphabetPos :: Char -> Maybe Int
alphabetPos c = elemIndex (toLower c) ['a'..'z']

-- | Finds the alphabet's nth character and wraps around larger indices.
--
-- __Note:__ Returns only lowercase letters.
--
-- prop> charAtPos =<< alphabetPos c = toLower c
charAtPos :: Int -> Maybe Char
charAtPos p = lookup (abs $ p `mod` 26) (zip [0..] ['a'..'z'])

-- | Generates a unique alphabet starting with the key, with all consecutively
-- duplicated characters removed.
--
-- >>> makeAlpha "hello"
-- "HELOABCDFGIJKMNPQRSTUVWXYZ"
--
-- prop> length (makeAlpha str) == 26
makeAlpha :: String -> String
makeAlpha k = nub (key ++ alpha)
    where
      alpha = ['A' .. 'Z']
      key = map toUpper =<< words k

