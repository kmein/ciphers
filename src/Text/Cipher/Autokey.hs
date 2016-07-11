-- | This module implements the so-called 'Autokey cipher' as used by the
-- American Cryptogram Association. See
-- <https://en.wikipedia.org/wiki/Autokey_cipher Wikipedia> for reference.
module Text.Cipher.Autokey where

import Text.Cipher.Vigenere

-- | Given a key and a text, applies a 'vigenere' shift to the text,
-- using the given key and a portion of the text.
--
-- prop> autokey key text = vigenere (key ++ text) text
autokey :: String -> String -> String
autokey key text = vigenere (key ++ text) text

-- | This function is the inverse of the 'autokey' function.
-- Given just a key, it is able to reconstruct the plaintext.
unautokey :: String -> String -> String
unautokey key text = loop key text []
    where
      loop k t res
          | null t = res
          | otherwise = loop decoded t' (res ++ decoded)
          where
            (known, t') = splitAt (length k) t
            decoded = unvigenere k known
