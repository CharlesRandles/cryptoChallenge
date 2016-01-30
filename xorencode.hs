module XorEncode where

import Data.Bits (xor)

import Crypto

xorEncode::Plain -> Key -> Cipher
xorEncode plain key = [xor (fst p) (snd p) | p <- zip plain (cycle key)]

xorEncodeText :: String -> String -> Cipher
xorEncodeText text key = xorEncode (fromText text) (fromText key)