-- | This module implements a basic Polybius Square cipher mapping a character
-- to a pair of integers from 1 to 5. These integers are based on the
-- character's position in a 5x5 alphabet square. See
-- <https://en.wikipedia.org/wiki/Polybius_square the Wikipedia article> for more
-- info.
module Text.Cipher.Polybius where

import Data.Char (toLower)
import Data.List (delete, elemIndex, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)
import Text.Cipher.Types

-- | Returns the Polybius-square-encrypted version of the plaintext in
-- form of digits in groups of two. Unknown characters are mapped to "00".
--
-- >>> polybius "hello world"
-- "32 51 13 13 43 00 25 43 24 13 41"
polybius :: Message Plain -> Message Cipher
polybius = unwords . map polybiusVal
    where
      table = chunksOf 5 (delete 'j' ['a'..'z'])
      otherTable = transpose table
      polybiusVal 'j' = polybiusVal 'i'
      polybiusVal c = show (extract table) ++ show (extract otherTable)
          where
            extract css =
              case filter isJust (map (elemIndex $ toLower c) css) of
                (Just n : _) -> n + 1
                _ -> 0

-- | This is basically the inverse of the 'polybius' function.
-- It is, however, not able to reconstruct the case information (upper/lower)
-- based on just the indices in the square. "00" is mapped to a space.
--
-- __Note:__ The input string has to consist entirely of digits in groups
-- of two.
unpolybius :: Message Cipher -> Message Plain
unpolybius = map unpolybiusVal . words
    where
      unpolybiusVal "00" = ' '
      unpolybiusVal (y:x:_) = table !! (x' - 1) !! (y' - 1)
          where
            [x', y'] = map (read . return) [x, y]
            table = chunksOf 5 (delete 'j' ['a'..'z'])
      unpolybiusVal _ = '\NUL'

