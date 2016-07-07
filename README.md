# ciphers

Meet `ciphers` the Swiss Army Knife of classical cryptography.

## Features
`ciphers` can encrypt and decrypt the following ciphers for you:
(Note: It does not actually crack them, you have to supply the key or passphrase yourself.)

* [Atbash](https://en.wikipedia.org/wiki/Atbash)
* [Caesar](https://en.wikipedia.org/wiki/Caesar_cipher) which can also be used for [ROT-13](https://en.wikipedia.org/wiki/Rot13)
* A "grid cipher" that substitutes letters with a two-digit code (taken from a 5x5-grid, see [Grid])
* [Playfair](https://en.wikipedia.org/wiki/Playfair_cipher)
* [Scytale](https://en.wikipedia.org/wiki/Scytale)
* [Vigenère](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher)

### Grid

Since the "grid cipher" is the only not so well known algorithm in `ciphers`, there is a small tutorial here.

```
x 1 2 3 4 5
1 A B C D E
2 F G H I K
3 L M N O P
4 Q R S T U
5 V W X Y Z
```
(`I/J` have the same position: `24`)

To encode a character of information, one combines the row and the column number of the character.
This is then done for all characters in the message. Note that this is a _very_ weak cipher and really 
should not be used.

## Building

* To build, run `stack build`. This will put an executable called `ciphers-exe` into 
  `.stack-work/dist/$ARCH_$OS/Cabal-$CABAL_VERSION/build/ciphers-exe/`.
* A user-wide installation can be issued with `stack install`.

## Usage

```
ciphers - a text filter for various cryptographic ciphers

Usage: ciphers-exe (-c|--cipher CIPHER[:KEY]) [-d|--decrypt]
  Encrypt/decrypt various cipher algorithms. Currently available: atbash,
  caesar:N, grid, playfair:KEY, scytale:N, vigenere:KEY.

Available options:
  -h,--help                Show this help text
  -c,--cipher CIPHER[:KEY] choose the cipher algorithm (one of "atbash",
                           "caesar", "grid", "playfair", "scytale" or
                           "vigenere")
  -d,--decrypt             act as decrypt filter
```

