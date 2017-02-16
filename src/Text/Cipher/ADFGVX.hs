{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module implements the German 'ADFGVX' cipher. See
-- <https://en.wikipedia.org/wiki/ADFGVX_cipher> for reference.
module Text.Cipher.ADFGVX where

import Data.Char (toUpper)
import Data.List (elemIndex, find, intersect, nub, sortOn, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Text.Cipher.Types
import Debug.Trace

-- | Given two keys (the first one being the polybius key and the second one
-- being used for fractionation/transposition, encrypts plaintext using the ADFGVX
-- cipher.
adfgvx :: Key -> Key -> Message Plain -> Message Cipher
adfgvx (map toUpper -> key1) (nub -> key2) = fractionation . squareLookup . normalisePlain
  where
    fractionation =
        concatMap snd .
        sortOn fst . zip key2 . transpose . chunksOf (length key2)
    squareLookup =
        let square = chunksOf 6 $ nub (key1 ++ ['A' .. 'Z'] ++ ['0' .. '9'])
        in concatMap (indexToAdfgvx . (`nestedElemIndex` square))
    nestedElemIndex x xss =
        case find ((x `elem`) . snd) $ zip [0 ..] xss of
            Just (i, xs) -> (i, fromMaybe (-1) $ elemIndex x xs)
            Nothing -> (-1, -1)
    normalisePlain = flip intersect (['A'..'Z'] ++ ['0'..'9']) . map toUpper
    indexToAdfgvx (x, y) = [toAdfgvx x, toAdfgvx y]
      where
        toAdfgvx =
            \case
                0 -> 'A'
                1 -> 'D'
                2 -> 'F'
                3 -> 'G'
                4 -> 'V'
                5 -> 'X'
