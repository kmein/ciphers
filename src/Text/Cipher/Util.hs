module Text.Cipher.Util where

import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

isVowel :: Char -> Bool
isVowel = flip elem "aeiouäöü"

index :: (Eq a) => a -> [a] -> Int
index c = fromJust . elemIndex c

alphabetPos :: Char -> Maybe Int
alphabetPos c = lookup (toLower c) (zip ['a'..'z'] [1..])

charAtPos :: Int -> Maybe Char
charAtPos p = lookup (abs p `mod` 26) (zip [1..] ['a'..'z'])

