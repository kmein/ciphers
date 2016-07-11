-- | This module implements the ancient Scytale transposition cipher (for more
-- info, see <https://en.wikipedia.org/wiki/Scytale Wikipedia>.
module Text.Cipher.Scytale where

-- | Given a perimeter for the scytale, transpose a text from
-- any character set.
--
-- >>> scytale 5 "какая сегодня погода"
-- к доаснгкеяоаг дяопа
scytale :: Int -> String -> String
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
unscytale :: Int -> String -> String
unscytale perimeter text = scytale perimeter' text
    where perimeter' = length text `div` perimeter

