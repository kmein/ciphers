# ciphers

Meet `ciphers`—the Swiss Army Knife of classical cryptography!

[![Build Status](https://travis-ci.org/kmein/ciphers.svg?branch=master)](https://travis-ci.org/kmein/ciphers)

## Features
`ciphers` can encrypt and decrypt the following ciphers for you:
(Note: It does not actually crack them, you have to supply the key or passphrase yourself.)

* [ADFGVX](https://en.wikipedia.org/wiki/ADFGVX_cipher)
* [Atbash](https://en.wikipedia.org/wiki/Atbash)
* [Autokey](https://en.wikipedia.org/wiki/Autokey_cipher)
* [Caesar](https://en.wikipedia.org/wiki/Caesar_cipher) which can also be used for [ROT-13](https://en.wikipedia.org/wiki/Rot13)
* [One Time Pad](https://en.wikipedia.org/wiki/One-time_pad), which generates a
  random key as long as the plaintext message, encrypts the plaintext (output on
  stdout) and outputs the key one stderr, which is useful for redirecting it to
  a file like `msg.key`.
* [Playfair](https://en.wikipedia.org/wiki/Playfair_cipher)
* [Polybius Square](https://en.wikipedia.org/wiki/Polybius_square)
* [Scytale](https://en.wikipedia.org/wiki/Scytale)
* [Substitution](#)
* [Vigenère](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher)

## Building

* To build, run `stack build`. This will put an executable called `ciphers` into
  `.stack-work/dist/$ARCH_$OS/Cabal-$CABAL_VERSION/build/ciphers/`.
* A user-wide installation can be issued with `stack install`.

## Usage

```
ciphers - a text filter for various cryptographic ciphers

Usage: ciphers COMMAND [-d|--decrypt] [-g|--grouped BLOCK-SIZE]
  Encrypt/decrypt various cipher algorithms.

Available options:
  -h,--help                Show this help text
  -d,--decrypt             Decrypt stdin
  -g,--grouped BLOCK-SIZE  Group output in evenly sized blocks

Available commands:
  atbash                   Atbash cipher
  autokey                  Autokey cipher
  caesar                   Caesar cipher
  onetimepad               One-time pad with random key. (Key is output on
                           stderr)
  playfair                 Playfair cipher
  polybius                 Polybius square
  scytale                  Scytale
  substitution             Alphabetical substitution
  vigenere                 Vigenère cipher
```

## Examples

```sh
% ciphers caesar -k3 <<< "Veni, vidi, vici."
Yhql, ylgl, ylfl.

% ciphers -d caesar -k3 <<< "Yhql, ylgl, ylfl."
Veni, vidi, vici.

% ciphers vigenere --key=snake <<< "meet me at elephant lake"
fsfe fs ly smpuaooe eolp

% ciphers -d vigenere --key=snake <<< "fsfe fs ly smpuaooe eolp"
meet me at elephant lake

% ciphers onetimepad <<< "hello world" 2> msg.key
wxjmc iaolo

% cat msg.key
osxanbllwnkx

% ciphers -g4 playfair -k example <<< "hide the gold in the treestump"
IKLM QNLO UGCK TZGX OSXA MOOZ PE
```
