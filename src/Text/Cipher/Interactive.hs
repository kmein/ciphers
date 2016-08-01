module Text.Cipher.Interactive where

import Text.Cipher

data Cipher
   = Atbash
   | Autokey String
   | Caesar Int
   | Polybius
   | OneTimePad
   | Playfair String
   | Scytale Int
   | Substitution String
   | Vigenere String
   deriving (Eq)

data Direction
   = Encrypt
   | Decrypt
   deriving (Eq)

data CiphersOptions
   = CiphersOptions
   { cipher :: Cipher
   , direction :: Direction
   , grouped :: Maybe Int
   }


toFunction :: CiphersOptions -> String -> String
toFunction (CiphersOptions c d g) =
    processGrouping g $
    case (c, d) of
      (Atbash           , Encrypt) -> atbash
      (Atbash           , Decrypt) -> unatbash
      (Autokey key      , Encrypt) -> autokey key
      (Autokey key      , Decrypt) -> unautokey key
      (Caesar shift     , Encrypt) -> caesar shift
      (Caesar shift     , Decrypt) -> uncaesar shift
      (Polybius         , Encrypt) -> polybius
      (Polybius         , Decrypt) -> unpolybius
      (Playfair key     , Encrypt) -> fromMaybe [] . playfair key
      (Playfair key     , Decrypt) -> fromMaybe [] . unplayfair key
      (Scytale perim    , Encrypt) -> scytale perim
      (Scytale perim    , Decrypt) -> unscytale perim
      (Substitution key , Encrypt) -> substitution key
      (Substitution key , Decrypt) -> unsubstitution key
      (Vigenere key     , Encrypt) -> vigenere key
      (Vigenere key     , Decrypt) -> unvigenere key

processGrouping :: Maybe Int -> (String -> String) -> String -> String
processGrouping g' f =
    case g' of
      Just g -> unwords . chunksOf g . concat . words . f
      Nothing -> f

