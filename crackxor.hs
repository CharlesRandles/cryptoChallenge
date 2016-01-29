module CrackXor where

import Data.List (sortBy)
import Data.Char

import Crypto
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

scoredDecodes :: Cipher -> [(Key, Double, Plain)]
scoredDecodes c = [(k, plaintextScore (toText p), p) | (k,p) <- decodes c]

minScore :: (Key, Double, Plain) -> (Key, Double, Plain) -> Ordering
minScore (_,x,_) (_,y,_) = compare x y

best cipher = head $ sortBy minScore $ scoredDecodes cipher

isNotRubbish :: String -> Bool
--Consider isPrint
isNotRubbish s = (all (\c -> (isAlphaNum c) || (isSpace c)) s)   && (madeOfWords s)

dodgies :: Cipher -> [(Key, Plain)]
dodgies c = filter (\(k, p) -> isNotRubbish (toText p)) $ decodes c

plaintext :: String
plaintext = "Almost all cryptography is less secure than people think."

ciphertext = xorEncode (fromText plaintext) (charToKey 'F')
