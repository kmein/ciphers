module Main (main) where

import Control.Monad (unless)
import Options.Applicative
import System.IO (stderr, hPutStr, hPutStrLn)
import System.Random (newStdGen)

import Text.Cipher (adfgvx, oneTimePad)
import Text.Cipher.Interactive

ciphersArgs :: Parser CiphersOptions
ciphersArgs =
    CiphersOptions
        <$> subparser
            (command "adfgvx"
                (info (uncurry ADFGVX <$> doubleKeyParser) $ progDesc "ADFGVX cipher")
            <> command "atbash"
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
      doubleKeyParser =
          (,)
          <$> strOption (long "key" <> short 'k'
              <> metavar "SQUARE KEY" <> help "use SQUARE KEY for the Polybius square")
          <*> strOption (long "fractionation" <> short 'f'
              <> metavar "FRACTIONATION KEY" <> help "use FRACTIONATION KEY for the transposition")

doCipher :: CiphersOptions -> IO ()
doCipher opts@(CiphersOptions c d g) =
    case c of
        OneTimePad ->
            unless (d == Decrypt) $
            do inp <- getContents
               (cipherText, key) <- oneTimePad inp <$> newStdGen
               putStrLn $ processGrouping g id cipherText
               hPutStrLn stderr key
        ADFGVX key1 key2 ->
            unless (d == Decrypt) $
            do inp <- getContents
               let cipherText = adfgvx key1 key2 inp
               putStrLn $ processGrouping g id cipherText
        _ -> interact $ toFunction opts

main :: IO ()
main = execParser opts >>= doCipher
    where
      opts =
          info (helper <*> ciphersArgs)
              (fullDesc
              <> progDesc "Encrypt/decrypt various cipher algorithms."
              <> header "ciphers - a text filter for various cryptographic ciphers")
