{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Data.Char
import Test.DocTest
import Test.Framework
import Text.Cipher

main :: IO ()
main = doctest ["src"] >> htfMain htf_thisModulesTests

test_atbash =
    do assertEqual (atbash "irk") "rip"
       assertEqual (atbash "low") "old"
       assertEqual (atbash "hob") "sly"
       assertEqual (atbash "hold") "slow"
       assertEqual (atbash "holy") "slob"
       assertEqual (atbash "horn") "slim"
       assertEqual (atbash "glow") "told"
       assertEqual (atbash "grog") "tilt"
       assertEqual (atbash "zoo") "all"
       assertEqual (atbash "wizard") "draziw"

prop_atbash xs = unatbash (atbash xs) == xs

test_autokey =
    do assertEqual
           (autokey "kilt" "meetatthefountain")
           "wmpmmxxaeyhbryoca"
       assertEqual
           (autokey "queenly" "attackatdawn")
           "qnxepvytwtwp"

test_unautokey =
    do assertEqual
           (unautokey "kilt" "wmpmmxxaeyhbryoca")
           "meetatthefountain"
       assertEqual
           (unautokey "queenly" "qnxepvytwtwp")
           "attackatdawn"

test_caesar =
    do assertEqual
           (caesar (-3) "the quick brown fox jumps over the lazy dog")
           "qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald"


test_uncaesar =
    do assertEqual
           (uncaesar (-3) "qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald")
           "the quick brown fox jumps over the lazy dog"

test_playfair =
    do assertEqual
           (playfair "playfairexample" "hide the gold in the treestump")
           (Just "BM OD ZB XD NA BE KU DM UI XM MO UV IF")

test_unplayfair =
    do assertEqual
           (unplayfair "playfairexample" "BM OD ZB XD NA BE KU DM UI XM MO UV IF")
           (Just "HI DE TH EG OL DI NT HE TR EX ES TU MP")

-- playfair, scytale, substitution

test_vigenere =
    do assertEqual
           (vigenere "lemon" "attackatdawn")
           "lxfopvefrnhr"

test_unvigenere =
    do assertEqual
           (unvigenere "lemon" "lxfopvefrnhr")
           "attackatdawn"

test_adfvgx =
    do assertEqual
           (adfgvx "na1c3h8tb2ome5wrpd4f6g7i9j0klqsuvxyz" "privacy" "attack at 1200 am")
           "DGDDDAGDDGAFADDFDADVDVFAADVX"
