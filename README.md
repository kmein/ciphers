# ciphers

Meet `ciphers`—the Swiss Army Knife of classical cryptography!

## Features
`ciphers` can encrypt and decrypt the following ciphers for you:
(Note: It does not actually crack them, you have to supply the key or passphrase yourself.)

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
* [Vigenère](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher)

## Building

* To build, run `stack build`. This will put an executable called `ciphers` into
  `.stack-work/dist/$ARCH_$OS/Cabal-$CABAL_VERSION/build/ciphers/`.
* A user-wide installation can be issued with `stack install`.

## Usage

```
ciphers - a text filter for various cryptographic ciphers

Usage: ciphers (-c|--cipher CIPHER[:KEY]) [-d|--decrypt]
               [-g|--grouped BLOCK-SIZE]
  Encrypt/decrypt various cipher algorithms. Currently available: atbash,
  autokey:KEY, caesar:N, onetimepad, playfair:KEY, polybius, scytale:N,
  vigenere:KEY.

Available options:
  -h,--help                Show this help text
  -c,--cipher CIPHER[:KEY] choose the cipher algorithm
  -d,--decrypt             act as decrypt filter
  -g,--grouped BLOCK-SIZE  group output in evenly sized blocks
```

## Examples

```sh
% ciphers -c caesar:3 <<< "Veni, vidi, vici."
Yhql, ylgl, ylfl.

% ciphers -d -c caesar:3 <<< "Yhql, ylgl, ylfl."
Veni, vidi, vici.

% ciphers -cvigenere:snake <<< "meet me at elephant lake"
fsfe fs ly smpuaooe eolp

% ciphers -d -cvigenere:snake <<< "fsfe fs ly smpuaooe eolp"
meet me at elephant lake

% ciphers -c onetimepad <<< "hello world" 2> msg.key
wxjmc iaolo

% cat msg.key
osxanbllwnkx

% ciphers -g4 -cplayfair:example <<< "hide the gold in the treestump"
IKLM QNLO UGCK TZGX OSXA TOYE EA
```
