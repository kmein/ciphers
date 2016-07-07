module Text.Cipher.Atbash where

import Data.Maybe (fromMaybe)

atbash :: String -> String
atbash = map atbashChar
    where
      atbashChar c = fromMaybe c $ lookup c dict
      dict = zip ['A'..'Z'] (reverse ['A'..'Z']) ++ zip ['a'..'z'] (reverse ['a'..'z'])

unatbash :: String -> String
unatbash = atbash

