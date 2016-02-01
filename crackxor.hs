module CrackXor where

import Data.List (sortBy, transpose)
import Data.List.Split (chunksOf)

import Data.Char

import CryptoChallenge
import Plaintext
import XorEncode
import Plaintext

keyAlphabet :: Key
--keyAlphabet = fromText (['a'..'z'] ++ ['A'..'Z'] )
keyAlphabet = [0..0xff]
--keyAlphabet = fromText ['a'..'z']
--keyAlphabet = charToKey '5'
 
decodes :: Cipher -> [(Key, Plain)]
decodes cipher = [([kc], xorEncode cipher [kc]) | kc <- keyAlphabet]

scoredDecodes :: Cipher -> [(Key, Score, Plain)]
scoredDecodes c = [(k, plaintextScore (toString p), p) | (k,p) <- decodes c]

minScore :: (Key, Double, Plain) -> (Key, Score, Plain) -> Ordering
minScore (_,x,_) (_,y,_) = compare x y

best cipher = head $ sortBy minScore $ scoredDecodes cipher

possibles :: Cipher -> [(Key, Score,  Plain)]
possibles c = filter (\(k, s, p) -> isNotRubbish (toString p)) $ scoredDecodes c

plaintext :: String
plaintext = "Almost all cryptography is less secure than people think."

splitToPairs :: Cipher -> Int -> [(Cipher, Cipher)]
splitToPairs cipher chunkSize
             | (length cipher) < 2 * chunkSize = []
             | otherwise = ((take chunkSize cipher),
                            (take chunkSize (drop chunkSize cipher))) 
                           : (splitToPairs (drop chunkSize cipher) chunkSize)

average :: [Int] -> Double
average xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)

hammingScan :: Cipher -> Int -> Double
hammingScan cipher chunkSize = (average [hammingDistance c1 c2 
                                   | (c1,c2) <- splitToPairs cipher chunkSize])
                                   / (fromIntegral chunkSize)
                                   
testKeyLengths ::Int -> Int -> Cipher -> [(Int, Double)]
testKeyLengths shortest longest cipher = sortBy (\(a,b) (c,d) -> compare b d)
                        [(keylen, hammingScan cipher keylen)
                        | keylen <- [shortest..longest]]

transposeChunks :: Cipher -> Int -> [Cipher]
transposeChunks cipher chunkSize = transpose $ chunksOf chunkSize cipher

regenKey :: [Cipher] -> Key
regenKey [] = []
regenKey (c:cs) = (head keyChar) : regenKey cs
         where (keyChar, _, _) = best c

crackRepeatXor cipher = regenKey $ transposeChunks cipher (fst $ head $ testKeyLengths 2 10 cipher)