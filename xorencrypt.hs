{- xor encoding -}

module XorEncrypt (xorEncrypt) where

import Data.Bits (xor)
import Crypto

xorEncrypt :: Plain -> Key -> Cipher
xorEncrypt a b =  map (\p -> (fst p) `xor` (snd p)) $ zip a b

