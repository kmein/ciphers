module Text.Cipher.OneTimePad where

import Control.Monad (replicateM)
import System.Random (RandomGen, randomRs)
import Text.Cipher.Vigenere

oneTimePad :: (RandomGen g) => String -> g -> (String, String)
oneTimePad str g = (vigenere key str, key)
    where
      key = randomKey (length str) g
      randomKey len g = take len $ randomRs ('a', 'z') g
