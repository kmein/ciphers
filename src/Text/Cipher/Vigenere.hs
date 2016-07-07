module Text.Cipher.Vigenere where

import Data.Maybe (fromMaybe)
import Text.Cipher.Util (charAtPos, alphabetPos)

vigenere :: String -> String -> String
vigenere = zipWith shift . cycle
    where
      shift k c =
          fromMaybe c $
          do cp <- alphabetPos c
             kp <- alphabetPos k
             charAtPos $ cp + kp

unvigenere :: String -> String -> String
unvigenere = zipWith unshift . cycle
    where
      unshift k c =
          fromMaybe c $
          do cp <- alphabetPos c
             kp <- alphabetPos k
             charAtPos $ cp - kp
