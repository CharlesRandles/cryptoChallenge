module XorEncode where

import Data.Bits (xor)

import Crypto

xorEncode::Plain -> Key -> Cipher
xorEncode plain key = [xor (fst p) (snd p) | p <- zip plain (cycle key)]
