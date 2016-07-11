-- | The One-Time Pad cipher is an idealised variant of the Vigenère cipher
-- implemented in "Text.Cipher.Vigenere". Once encrypted, it is impossible to crack
-- without the whole key. More details can be found in
-- <https://en.wikipedia.org/wiki/One-time_pad the Wikipedia article>.
module Text.Cipher.OneTimePad where

import Control.Monad (replicateM)
import System.Random (RandomGen, randomRs)
import Text.Cipher.Vigenere

-- | Given a generator with which to generate the random key and a plaintext,
-- returns the cipher text and the key it was encrypted with.
oneTimePad :: (RandomGen g) => String -> g -> (String, String)
oneTimePad str g = (vigenere key str, key)
    where
      key = randomKey (length str) g
      randomKey len g = take len $ randomRs ('a', 'z') g
