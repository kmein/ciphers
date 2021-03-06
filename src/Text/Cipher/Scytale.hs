-- | This module implements the ancient Scytale transposition cipher (for more
-- info, see <https://en.wikipedia.org/wiki/Scytale Wikipedia>.
module Text.Cipher.Scytale where

import Text.Cipher.Types

-- | Given a perimeter for the scytale, transpose a text from
-- any character set.
--
-- >>> scytale 5 "i like trains"
-- "iei  nltsir ka "
scytale :: Int -> Message Plain -> Message Cipher
scytale perimeter text =
    do let text' = ensure perimeter text
       x <- [0..(perimeter - 1)]
       y <- [x, (x + perimeter) .. (length text' - 1)]
       return (text' !! y)
    where
      ensure perimeter text
          | 0 <- length text `mod` perimeter = text
          | otherwise = ensure perimeter (text ++ " ")

-- | This is the inverse operation to the 'scytale' function.
unscytale :: Int -> Message Cipher -> Message Plain
unscytale perimeter text = scytale perimeter' text
    where perimeter' = length text `div` perimeter

