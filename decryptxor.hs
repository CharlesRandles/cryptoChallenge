{- Decrypt a string that has been repeatedly xor'd with a single char -}

module DecryptXor  where

import Data.Char (ord, chr)

import Crypto
import XorEncrypt
import Plaintext

candidates :: Int -> [Key]
candidates n = map fromText [replicate n c | c <- ['a'..'z']++['A'..'Z']++['0'..'9']]

decrypts :: Cipher -> [(Key, Plain)]
decrypts cipher = [(key, xorEncrypt cipher key) | key <- candidates (length cipher)]

plaintexts = filter (\p -> isPlaintext (snd p))

findKey cipher = map (head.fst) $ filter (\p -> isPlaintext $ snd p) $ decrypts cipher

xor_ciphertext = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

decryptXor c = plaintexts $ decrypts c