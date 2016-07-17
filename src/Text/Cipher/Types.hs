{-# LANGUAGE EmptyDataDecls #-}
-- | This module contains some type synonyms to improve type readability.
module Text.Cipher.Types where

-- | A phantom type for plaintext.
data Plain

-- | A phantom type for ciphertext.
data Cipher

-- | An encrypted or decrypted message.
type Message a = String

-- | A password / key for ciphers like Vigenère.
type Key = String
