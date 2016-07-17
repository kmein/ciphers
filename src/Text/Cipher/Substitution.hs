-- | This module implements a simple substitution cipher in which the alphabet
-- is encoded using a given key and then the characters are looked up.
--
-- For example, an alphabet encoded with hello would look like
-- "HELOABCDFGIJKMNPQRSTUVWXYZ".
-- Then, "A" would be "H" and "B" would be "E".
module Text.Cipher.Substitution where

import Data.Char
import Data.Maybe
import Text.Cipher.Types
import Text.Cipher.Util (makeAlpha)

-- | Given a key, encrypts a message using the simple substitution cipher
-- described above.
substitution :: Key -> Message Plain -> Message Cipher
substitution key = map $ \c -> fromMaybe c (lookup c dict)
    where
      alpha = makeAlpha key
      dict = zip ['A'..'Z'] alpha ++ zip ['a'..'z'] (map toLower alpha)

-- | Decrypts a ciphertext using a key. Essentially the inverse of 'substitution'.
unsubstitution :: Key -> Message Cipher -> Message Plain
unsubstitution key = map $ \c -> fromMaybe c (lookup c dict)
    where
      alpha = makeAlpha key
      dict = zip alpha ['A'..'Z'] ++ zip (map toLower alpha) ['a'..'z']
