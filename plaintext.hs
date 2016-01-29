{- Heuristics for detecting plaintext -}

module Plaintext where

import Data.Char

import Crypto

type FreqMap = [(Char, Int)]
type FreqPair = (Char, Char)

toText = map chr
fromText = map ord

isPlaintext :: Plain -> Bool
isPlaintext s = True
                && (plainTextScore s > 2)
              --  && (letterPercentage s > 0.80) 
             --   && (avgWordLength s < 10.0) -- || True

isKey s = True
          && (letterPercentage s > 0.9)

plainTextScore s = freqScore (letterFreqs s) expecteds

expecteds :: [FreqPair]
expecteds = [('e', 't'), -- e is more frequent than s
             ('t', 'a'),
             ('a', 'o'),
             ('o', 'i'),
             ('i', 'n'),
             ('n', 's'),
             ('s', 'h'),
             ('h', 'r'),
             ('r', 'd'),
             ('d', 'l'),
             ('l', 'c')]

freqScore :: FreqMap -> [FreqPair] -> Int
freqScore m e = freqScore' 0 m e
freqScore' :: Int -> FreqMap -> [FreqPair] -> Int
freqScore' n m [] = n
freqScore' n m (p:ps) = if frequency m (fst p) > frequency m (snd p) 
                      then freqScore' (n+1) m ps
                      else freqScore' (n-1) m ps

occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (\e -> x == e)

letterFreqs :: Plain -> FreqMap
letterFreqs s = [(c, occurrences c (map toLower (toText s))) | c <- ['a'..'z']]

frequency :: FreqMap -> Char -> Int
frequency map char = snd $ head $ filter (\p -> fst p == char) map

isLegitChar c = (isAlphaNum c) || (c `elem` " !@#$%^&*()-=_+{}[];:,<.>/?") 

letterPercentage s = (fromIntegral $ length (filter isLegitChar (toText s))) / (fromIntegral  $ length s)

avgWordLength s = ((fromIntegral . length) s) / ((fromIntegral .length . words) (toText s))