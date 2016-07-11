-- | This module, as the name suggests, serves as a utility module for ciphers
-- like the Vigenère cipher, which depends upon shifting alphabet characters by
-- some amount.
module Text.Cipher.Util where

import Data.Char (toLower)
import Data.List (elemIndex)

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

