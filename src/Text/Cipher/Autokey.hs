module Text.Cipher.Autokey where

import Text.Cipher.Vigenere

autokey :: String -> String -> String
autokey key text = vigenere (key ++ text) text

unautokey :: String -> String -> String
unautokey key text = loop key text []
    where
      loop k t res
          | null t = res
          | otherwise = loop decoded t' (res ++ decoded)
          where
            (known, t') = splitAt (length k) t
            decoded = unvigenere k known
