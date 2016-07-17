module Main (main) where

import Control.Monad (unless)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Options.Applicative
import System.IO (stderr, hPutStr)
import System.Random (newStdGen)
import Text.Cipher
    ( atbash, unatbash
    , autokey, unautokey
    , caesar, uncaesar
    , polybius, unpolybius
    , oneTimePad
    , playfair, unplayfair
    , scytale, unscytale
    , substitution, unsubstitution
    , vigenere, unvigenere
    )

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

ciphersArgs :: Parser CiphersOptions
ciphersArgs =
    CiphersOptions
        <$> subparser
            (command "atbash"
                (info (pure Atbash) $ progDesc "Atbash cipher")
            <> command "autokey"
                (info (Autokey <$> keyParser) $ progDesc "Autokey cipher")
            <> command "caesar"
                (info ((Caesar . read) <$> keyParser) $ progDesc "Caesar cipher")
            <> command "onetimepad"
                (info (pure OneTimePad) $ progDesc "One-time pad with random key. (Key is output on stderr)")
            <> command "playfair"
                (info (Playfair <$> keyParser) $ progDesc "Playfair cipher")
            <> command "polybius"
                (info (pure Polybius) $ progDesc "Polybius square")
            <> command "scytale"
                (info ((Scytale . read) <$> keyParser) $ progDesc "Scytale")
            <> command "substitution"
                (info (Substitution <$> keyParser) $ progDesc "Alphabetical substitution")
            <> command "vigenere"
                (info (Vigenere <$> keyParser) $ progDesc "Vigenère cipher"))
        <*> flag Encrypt Decrypt
            (long "decrypt" <> short 'd'
            <> help "Decrypt stdin")
        <*> optional (option auto
            (long "grouped" <> short 'g' <> metavar "BLOCK-SIZE"
            <> help "Group output in evenly sized blocks"))
    where
      keyParser =
          strOption (long "key" <> short 'k' <> metavar "KEY"
          <> help "use KEY for encryption")

doCipher :: CiphersOptions -> IO ()
doCipher (CiphersOptions c d g) =
    if c == OneTimePad
       then unless (d == Decrypt) $
            do inp <- getContents
               (cipherText, key) <- oneTimePad inp <$> newStdGen
               putStr cipherText
               hPutStr stderr key
       else interact $ processGrouping g $
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
    where
      processGrouping g' f =
          case g' of
            Just g -> unwords . chunksOf g . concat . words . f
            Nothing -> f

main :: IO ()
main = execParser opts >>= doCipher
    where
      opts =
          info (helper <*> ciphersArgs)
              (fullDesc
              <> progDesc "Encrypt/decrypt various cipher algorithms."
              <> header "ciphers - a text filter for various cryptographic ciphers")
