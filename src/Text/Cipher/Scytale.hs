module Text.Cipher.Scytale where

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

unscytale :: Int -> String -> String
unscytale perimeter text = scytale perimeter' text
    where perimeter' = length text `div` perimeter

