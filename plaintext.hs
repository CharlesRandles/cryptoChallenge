module Plaintext where

import Data.Char (toLower,
                  isPrint,
                  isAlphaNum,
                  isSpace)
import Data.Maybe (fromJust)

import CryptoChallenge

type IntFreqMap = [(Char, Int)]
type FreqMap = [(Char, Double)]
type Score = Double 
alphabet :: [Char]
alphabet = [' '] ++ ['a'..'z']

threshold :: Double
threshold = 1.7

minValidChar = 0x20
maxValidChar = 0x80

isPlaintext :: String -> Bool
isPlaintext s = ((chiSquaredString s englishFreqs) < threshold)
 -- (noCtrlChars (fromText s)) && 

avgWordLength s = (fromIntegral $ length s ) / fromIntegral (length $ words s)

madeOfWords :: String -> Bool
madeOfWords s = (avgWordLength s > 2.0) && (avgWordLength s < 10.0)

noCtrlChars :: Plain -> Bool
noCtrlChars p = not $ any (\c -> (c <= minValidChar) || (c >= maxValidChar)) p

isNotRubbish :: String -> Bool
--Consider isPrint
isNotRubbish s = (all (\c -> (isAlphaNum c) || (isSpace c)) s)   && (madeOfWords s)

justLookup c cs= fromJust $ lookup c cs

occurrences :: Eq a => a -> [a] -> Int
occurrences a as = length $ filter (\c -> c == a)  as

freqs :: String -> IntFreqMap
freqs s = [(c, occurrences c (map toLower s)) | c <- alphabet]

actualFreqs :: String -> FreqMap
actualFreqs s= [(c, (fromIntegral n) / l) | (c,n) <- freqs s]
               where l = fromIntegral $ length s

chiSquared :: [(Double, Double)] -> Score
chiSquared dataSet = sum [((o - e) * (o - e)) / e | (o,e) <- dataSet]

chiSquaredString :: String -> FreqMap -> Score
chiSquaredString string langFreqs =
  chiSquared [((justLookup c o), (justLookup c e))
             | c <- alphabet]
  where o = actualFreqs string
        e = langFreqs

plaintextScore s = chiSquaredString s englishFreqs

{- from http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html -}
englishFreqs :: FreqMap
englishFreqs = [(' ',  0.121),
                ('e', 0.1202 ),
                ('t', 0.0910 ),
                ('a', 0.0812 ),
                ('o', 0.0768 ),
                ('i', 0.0731 ),
                ('n', 0.0695 ),
                ('s', 0.0628 ),
                ('r', 0.0602 ),
                ('h', 0.0592 ),
                ('d', 0.0432 ),
                ('l', 0.0398 ),
                ('u', 0.0288 ),
                ('c', 0.0271 ),
                ('m', 0.0261 ),
                ('f', 0.0230 ),
                ('y', 0.0211 ),
                ('w', 0.0209 ),
                ('g', 0.0203 ),
                ('p', 0.0182 ),
                ('b', 0.0149 ),
                ('v', 0.0111 ),
                ('k', 0.0069 ),
                ('x', 0.0017 ),
                ('q', 0.0011 ),
                ('j', 0.0010 ),
                ('z', 0.0007 )]

testTexts :: [String]
testTexts = ["Now is the winter of our discontent, made glorious summer by this son of York"
            ,"This challenge isn't conceptually hard, but it involves actual error-prone coding. The other challenges in this set are there to bring you up to speed."
            ,"wokka wokka!!!"
            ,"qjtpgfdvggoteklwzvndfstbguwvpycwgfbtermtquoxmsleybzqruyvispfouzaskekzc"
            ,"bwipzyaesdtxannkfhexhlcinfitbqgycubtcograrddvxxojltgyuhzwhpwnfyjospwbc"
            ,"Nous n'avons pas des aliments, at j'ai tres, tres faim."
            ,"Wer reitet so spät durch Nacht und Wind? Es ist der Vater mit seinem Kind"
            ,"Dans ce pays-ci, il est bon de tuer de temps en temps un amiral pour encourager les autres"
            ]
