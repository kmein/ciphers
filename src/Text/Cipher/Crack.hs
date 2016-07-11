-- | This module will eventually be used for implementing some kind of
-- statistical analysis and automatical cracking functionality for the
-- Caesar and Vigenère ciphers.
--
-- For now, there is /nothing to see/ here. So feel free to go on.
module Text.Cipher.Crack () where

countChars :: String -> String -> Map.Map Char Int
countChars relevants = foldr (Map.adjust (+ 1)) startMap
    where startMap = Map.fromList (zip relevants (repeat 0))

letterFrequencies :: Map.Map Char Int -> Map.Map Char Double
letterFrequencies theMap = fmap ((*100) . (/ count) . fromIntegral) theMap
    where count = fromIntegral $ sum (Map.elems theMap)
