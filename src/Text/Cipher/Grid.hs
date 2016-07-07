module Text.Cipher.Grid where

import Data.Char (toLower)
import Data.List (delete, elemIndex, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

grid :: String -> String
grid = unwords . map gridVal
    where
      table = chunksOf 5 (delete 'j' ['a'..'z'])
      otherTable = transpose table
      gridVal 'j' = gridVal 'i'
      gridVal c = show (extract table) ++ show (extract otherTable)
          where
            extract css =
              case filter isJust (map (elemIndex $ toLower c) css) of
                (Just n : _) -> n + 1
                _ -> 0

ungrid :: String -> String
ungrid = map ungridVal . words
    where
      ungridVal "00"    = ' '
      ungridVal _       = '\NUL'
      ungridVal (y:x:_) = table !! (x' - 1) !! (y' - 1)
          where
            [x', y'] = map (read . return) [x, y]
            table = chunksOf 5 (delete 'j' ['a'..'z'])

