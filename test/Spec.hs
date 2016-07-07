{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Data.Char
import Test.Framework
import Text.Cipher

main :: IO ()
main = htfMain htf_thisModulesTests

prop_caesar xs =
    forAll arbitrary $ \n ->
        map toLower xs == uncaesar n (caesar n xs)

prop_atbash xs = xs == unatbash (atbash xs)

prop_scytale xs =
    forAll arbitrary $ \n ->
        (n > 0) && (n == length xs) ==>
            xs == unscytale n (scytale n xs)

prop_vigenere xs =
    forAll arbitrary $ \key ->
        all (`elem` ['a'..'z']) (key ++ xs) ==>
            xs == unvigenere key (vigenere key xs)
