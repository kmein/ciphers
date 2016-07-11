module Main (main) where

import Control.Monad (unless)
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
   | Vigenere String
   deriving (Eq)

instance Read Cipher where
    readsPrec prec r =
        tryParse [("atbash", Atbash), ("polybius", Polybius), ("onetimepad", OneTimePad)] r
            ++ [(Autokey key, u)
                | ("autokey", s) <- lex r
                , (":", t) <- lex s
                , (key, u) <- lex t]
            ++ [(Caesar n, u)
                | ("caesar", s) <- lex r
                , (":", t) <- lex s
                , (n, u) <- readsPrec (prec+1) t]
            ++ [(Playfair key, u)
                | ("playfair", s) <- lex r
                , (":", t) <- lex s
                , (key, u) <- lex t]
            ++ [(Scytale n, u)
                | ("scytale", s) <- lex r
                , (":", t) <- lex s
                , (n, u) <- readsPrec (prec+1) t]
            ++ [(Vigenere key, u)
                | ("vigenere", s) <- lex r
                , (":", t) <- lex s
                , (key, u) <- lex t]
        where
          tryParse [] r = []
          tryParse ((attempt, result):xs) r =
              if take (length attempt) r == attempt
                 then [(result, drop (length attempt) r)]
                 else tryParse xs r

data Direction
   = Encrypt
   | Decrypt
   deriving (Eq)

data CiphersOptions = CiphersOptions Cipher Direction

ciphersArgs :: Parser CiphersOptions
ciphersArgs =
    CiphersOptions
        <$> option auto
            (long "cipher" <> short 'c' <> metavar "CIPHER[:KEY]"
            <> help "choose the cipher algorithm")
        <*> flag Encrypt Decrypt
            (long "decrypt" <> short 'd'
            <> help "act as decrypt filter")

doCipher :: CiphersOptions -> IO ()
doCipher (CiphersOptions c d) =
    if c == OneTimePad
       then unless (d == Decrypt) $
            do inp <- getContents
               (cipherText, key) <- oneTimePad inp <$> newStdGen
               putStr cipherText
               hPutStr stderr key
       else interact $
            case (c, d) of
              (Atbash        , Encrypt) -> atbash
              (Atbash        , Decrypt) -> unatbash
              (Autokey key   , Encrypt) -> autokey key
              (Autokey key   , Decrypt) -> unautokey key
              (Caesar shift  , Encrypt) -> caesar shift
              (Caesar shift  , Decrypt) -> uncaesar shift
              (Polybius      , Encrypt) -> polybius
              (Polybius      , Decrypt) -> unpolybius
              (Playfair key  , Encrypt) -> fromMaybe [] . playfair key
              (Playfair key  , Decrypt) -> fromMaybe [] . unplayfair key
              (Scytale perim , Encrypt) -> scytale perim
              (Scytale perim , Decrypt) -> unscytale perim
              (Vigenere key  , Encrypt) -> vigenere key
              (Vigenere key  , Decrypt) -> unvigenere key


main :: IO ()
main = execParser opts >>= doCipher
    where
      opts =
          info (helper <*> ciphersArgs)
              (fullDesc
              <> progDesc "Encrypt/decrypt various cipher algorithms.\n\
              \Currently available: atbash, autokey:KEY, caesar:N, onetimepad, playfair:KEY, polybius, scytale:N, vigenere:KEY."
              <> header "ciphers - a text filter for various cryptographic ciphers")
