module Text.Cipher (module Cipher) where

import Text.Cipher.ADFGVX       as Cipher (adfgvx)
import Text.Cipher.Atbash       as Cipher (atbash, unatbash)
import Text.Cipher.Autokey      as Cipher (autokey, unautokey)
import Text.Cipher.Caesar       as Cipher (caesar, uncaesar)
import Text.Cipher.Polybius     as Cipher (polybius, unpolybius)
import Text.Cipher.OneTimePad   as Cipher (oneTimePad)
import Text.Cipher.Playfair     as Cipher (playfair, unplayfair)
import Text.Cipher.Scytale      as Cipher (scytale, unscytale)
import Text.Cipher.Substitution as Cipher (substitution, unsubstitution)
import Text.Cipher.Vigenere     as Cipher (vigenere, unvigenere)

--
-- square :: String -> String
-- square = unwords . transpose . makeSublists '.' . concat . words
--     where
--       fill :: Int -> a -> [a] -> [a]
--       fill n ch xs
--           | length xs >= n = xs
--           | otherwise = fill n ch (xs ++ [ch])
--
--       makeSublists :: a -> [a] -> [[a]]
--       makeSublists ch xs =
--           chunksOf (ceiling $ sqrt $ fromIntegral len) $
--           fill (nextSquare len) ch xs
--           where
--             len = length xs
--             nextSquare n = ceiling (sqrt $ fromIntegral n) ^ 2
--
-- blockXOR :: String -> String -> String
-- blockXOR = zipWith $ curry (chr . uncurry xor . both ord)
--     where both f (x, y) = (f x, f y)
--
-- pigLatin :: String -> String
-- pigLatin [] = []
-- pigLatin word@(c:cs)
--     | isVowel c = word ++ "ay"
--     | otherwise = cs ++ (c:"ay")
--
-- TODO migrate `Geheimsprachen.hs`
