{- Implementing repeating key xor obfuscation -}

module Challenge5 where

import Crypto
import XorEncode
import Hex

text = "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"
key = "ICE"

ciphertext = hexEncode $ xorEncodeText text key