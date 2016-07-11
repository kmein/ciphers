module Text.Cipher.Polybius where

import Data.Char (toLower)
import Data.List (delete, elemIndex, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

polybius :: String -> String
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

unpolybius :: String -> String
unpolybius = map unpolybiusVal . words
    where
      unpolybiusVal "00"    = ' '
      unpolybiusVal (y:x:_) = table !! (x' - 1) !! (y' - 1)
          where
            [x', y'] = map (read . return) [x, y]
            table = chunksOf 5 (delete 'j' ['a'..'z'])
      unpolybiusVal _       = '\NUL'

