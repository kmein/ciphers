module Main (main) where

import Data.Maybe (fromMaybe)
import Options.Applicative
import Text.Cipher
    ( atbash, unatbash
    , caesar, uncaesar
    , grid, ungrid
    , playfair, unplayfair
    , scytale, unscytale
    , vigenere, unvigenere
    )

data Cipher
   = Atbash
   | Caesar Int
   | Grid
   | Playfair String
   | Scytale Int
   | Vigenere String

instance Read Cipher where
    readsPrec prec r =
        tryParse [("atbash", Atbash), ("grid", Grid)] r
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

data Direction = Encrypt | Decrypt

data CiphersOptions = CiphersOptions Cipher Direction

ciphersArgs :: Parser CiphersOptions
ciphersArgs =
    CiphersOptions
        <$> option auto
            (long "cipher" <> short 'c' <> metavar "CIPHER[:KEY]"
            <> help "choose the cipher algorithm (one of \"atbash\", \
            \\"caesar\", \"grid\", \"playfair\", \"scytale\" or \"vigenere\")")
        <*> flag Encrypt Decrypt
            (long "decrypt" <> short 'd'
            <> help "act as decrypt filter")

doCipher :: CiphersOptions -> IO ()
doCipher opts =
    interact $
    case opts of
      CiphersOptions Atbash          Encrypt -> atbash
      CiphersOptions Atbash          Decrypt -> unatbash
      CiphersOptions (Caesar shift)  Encrypt -> caesar shift
      CiphersOptions (Caesar shift)  Decrypt -> uncaesar shift
      CiphersOptions Grid            Encrypt -> grid
      CiphersOptions Grid            Decrypt -> ungrid
      CiphersOptions (Playfair key)  Encrypt -> fromMaybe [] . playfair key
      CiphersOptions (Playfair key)  Decrypt -> fromMaybe [] . unplayfair key
      CiphersOptions (Scytale perim) Encrypt -> scytale perim
      CiphersOptions (Scytale perim) Decrypt -> unscytale perim
      CiphersOptions (Vigenere key)  Encrypt -> vigenere key
      CiphersOptions (Vigenere key)  Decrypt -> unvigenere key


main :: IO ()
main = execParser opts >>= doCipher
    where
      opts =
          info (helper <*> ciphersArgs)
              (fullDesc
              <> progDesc "Encrypt/decrypt various cipher algorithms.\n\
              \Currently available: atbash, caesar:N, grid, playfair:KEY, scytale:N, vigenere:KEY."
              <> header "ciphers - a text filter for various cryptographic ciphers")
