module CrackXor where

import Data.List (sortBy)

import Crypto
import Plaintext
import XorEncode

keyAlphabet :: [Char]
keyAlphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

decodes :: Ciphertext -> [(Char, Plaintext)]
decodes cipher = [(c, xorEncode cipher (charToKey c)) | c <- keyAlphabet]

cp (c1, p1) (c2, p2) = compare
                       (chiSquaredString (toText p1)) (chiSquaredString (toText p2))

guesses cipher = sortBy cp
                 $  filter (\(c, pt) -> isPlaintext (toText pt))
                 (decodes cipher)

textGuesses cipher = map (\(k, pt) -> (k, (toText pt))) (guesses cipher)

plaintext :: String
plaintext = "Almost all cryptography is less secure than people think."

ciphertext = xorEncode (fromText plaintext) (charToKey 'F')
