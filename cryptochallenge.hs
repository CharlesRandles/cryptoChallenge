{- Some types and utilities for cryptography -}

module CryptoChallenge where

import Data.Char (ord, chr)
import Data.Bits (xor, (.&.), shiftR)
import Data.List
import Data.List.Split

type Cipher = [Int]
type Key = [Int]
type Plain = [Int]

aesBlocksize :: Int
aesBlocksize = 0x10 -- Because the standard says so

fromString = map ord
toString = map chr

charToKey :: Char -> [Int]
charToKey c = [ord c]

{- Hamming Distance -}
bitcount :: Int -> Int
bitcount 0 = 0
bitcount n = (n .&. 1) + (bitcount (shiftR n 1))

hamming :: Int -> Int -> Int
hamming a b = bitcount (a `xor` b)

hammingDistance :: Cipher -> Cipher -> Int
hammingDistance s1 s2 = sum $ map (\p -> hamming (fst p) (snd p))
                            (zip s1 s2)

hasDuplicateChunks :: Eq a => Int -> [a] -> Bool
hasDuplicateChunks chunkSize xs =
  (length chunks) /= (length $ nub chunks)
  where chunks = chunksOf chunkSize xs
