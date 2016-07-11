-- | This module implements the Vigenère cipher, a simple polyalphabetic
-- substitution conceived by G. B. Bellaso / B. de Vigenère. For more info, see
-- <https://en.wikipedia.org/wiki/Vigenère_cipher the Wikipedia page>.
module Text.Cipher.Vigenere where

import Data.Maybe (fromMaybe)
import Text.Cipher.Util (charAtPos, alphabetPos)

-- | Given a passphrase, encrypts the given string using the Vigenère encryption
-- method. Note that for optimal security, the key has to be as long as the message
-- (see "Text.Cipher.OneTimePad"). Otherwise, it is stretched over the whole
-- message by means of 'cycle'.
--
-- __Note__: Because this function internally uses  'charAtPos', it will always
-- return a lower cased cipher text. Unknown characters are not encrypted.
vigenere :: String -> String -> String
vigenere = zipWith shift . cycle
    where
      shift k c =
          fromMaybe c $
          do cp <- alphabetPos c
             kp <- alphabetPos k
             charAtPos $ cp + kp

-- | This function is essentially the inverse of the above 'vigenere' function.
--
-- __Note:__ Unknown characters are not encrypted. Upper case is transformed
-- into lower case.
unvigenere :: String -> String -> String
unvigenere = zipWith unshift . cycle
    where
      unshift k c =
          fromMaybe c $
          do cp <- alphabetPos c
             kp <- alphabetPos k
             charAtPos $ cp - kp
