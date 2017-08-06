import Text.Cipher

import Data.Char
import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  defaultMain $ testGroup "Tests" tests
  doctest ["src"]

tests :: [TestTree]
tests =
  [ testGroup
      "Atbash"
      [ testCase "atbash.1" ("rip" @?= atbash "irk")
      , testCase "atbash.2" ("old" @?= atbash "low")
      , testCase "atbash.3" ("sly" @?= atbash "hob")
      , testCase "atbash.4" ("slow" @?= atbash "hold")
      , testCase "atbash.5" ("slob" @?= atbash "holy")
      , testCase "atbash.6" ("slim" @?= atbash "horn")
      , testCase "atbash.7" ("told" @?= atbash "glow")
      , testCase "atbash.8" ("tilt" @?= atbash "grog")
      , testCase "atbash.9" ("all" @?= atbash "zoo")
      , testCase "atbash.10" ("draziw" @?= atbash "wizard")
      , testProperty "atbash.reverse" $ \xs ->
          atbash (reverse xs) == reverse (atbash xs)
      , testProperty "atbash.cancel" $ \xs -> unatbash (atbash xs) == xs
      ]
  , let pass1 = "kilt"
        plain1 = "meetatthefountain"
        cipher1 = "wmpmmxxaeyhbryoca"
        pass2 = "queenly"
        plain2 = "attackatdawn"
        cipher2 = "qnxepvytwtwp"
    in testGroup
         "Autokey"
         [ testCase "autokey.1" (cipher1 @?= autokey pass1 plain1)
         , testCase "unautokey.1" (plain1 @?= unautokey pass1 cipher1)
         , testCase "autokey.2" (cipher2 @?= autokey pass2 plain2)
         , testCase "unautokey.2" (plain2 @?= unautokey pass2 cipher2)
         ]
  , let pass = (-3)
        plain = "the quick brown fox jumps over the lazy dog"
        cipher = "qeb nrfzh yoltk clu grjmp lsbo qeb ixwv ald"
    in testGroup
         "Caesar"
         [ testCase "caesar" (cipher @?= caesar pass plain)
         , testCase "uncaesar" (plain @?= uncaesar pass cipher)
         , testProperty "caesar.negative" $ \n xs ->
             caesar n xs == uncaesar (-n) xs
         , testProperty "caesar.reverse" $ \n xs ->
             reverse (caesar n xs) == caesar n (reverse xs)
         , testProperty "caesar.cancel" $ \n xs ->
             uncaesar n (caesar n xs) == xs
         ]
  , let pass = "playfairexample"
        cipher = "BM OD ZB XD NA BE KU DM UI XM MO UV IF"
    in testGroup
         "Playfair"
         [ testCase
             "playfair"
             (Just cipher @?= playfair pass "hide the gold in the treestump")
         , testCase
             "unplayfair"
             (Just "HI DE TH EG OL DI NT HE TR EX ES TU MP" @?=
              unplayfair pass cipher)
         ]
  , testGroup "Scytale" [] -- TODO add tests
  , testGroup "Substitution" [] -- TODO add tests
  , let plain = "attackatdawn"
        pass = "lemon"
        cipher = "lxfopvefrnhr"
    in testGroup
         "Vigenere"
         [ testCase "vigenere" (cipher @?= vigenere pass plain)
         , testCase "unvigenere" (plain @?= unvigenere pass cipher)
         ]
  , testGroup
      "ADFVGX"
      [ testCase
          "adfgvx"
          ("DGDDDAGDDGAFADDFDADVDVFAADVX" @?=
           adfgvx
             "na1c3h8tb2ome5wrpd4f6g7i9j0klqsuvxyz"
             "privacy"
             "attack at 1200 am")
      ]
  ]
