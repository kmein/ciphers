module Text.Cipher.Util where

import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

isVowel :: Char -> Bool
isVowel = flip elem "aeiouäöü"

index :: (Eq a) => a -> [a] -> Int
index c = fromJust . elemIndex c

alphabetPos :: Char -> Maybe Int
alphabetPos c = elemIndex (toLower c) ['a'..'z']

charAtPos :: Int -> Maybe Char
charAtPos p = lookup (abs $ p `mod` 26) (zip [0..] ['a'..'z'])

