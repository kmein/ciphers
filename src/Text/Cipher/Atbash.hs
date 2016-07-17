-- | This module supplies functions for the easy-to-crack Atbash cipher
-- (see <https://en.wikipedia.org/wiki/Atbash Wikipedia> for reference).
module Text.Cipher.Atbash where

import Data.Maybe (fromMaybe)
import Text.Cipher.Types

-- | The Atbash cipher, as a simple substitution cipher, encrypts letters
-- by mapping them to the same position in a reversed standard Latin alphabet
-- (i. e. A to Z, B to Y, etc.).
atbash :: Message Plain -> Message Cipher
atbash = map atbashChar
    where
      atbashChar c = fromMaybe c $ lookup c dict
      dict = zip ['A'..'Z'] (reverse ['A'..'Z']) ++ zip ['a'..'z'] (reverse ['a'..'z'])

-- | This function is the reason why the Atbash cipher is so easy-to-crack:
-- Like with <https://en.wikipedia.org/wiki/ROT13 ROT-13>, the encryption
-- function also functions as a decryptor, so the following properties hold.
--
-- prop> atbash (atbash str) = str
-- prop> atbash = unatbash
unatbash :: Message Cipher -> Message Plain
unatbash = atbash

