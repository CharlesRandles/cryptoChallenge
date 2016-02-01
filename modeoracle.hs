{- http://cryptopals.com/sets/2/challenges/11/ -}
module ModeOracle where

import Data.ByteString as B (ByteString)
import Data.ByteString.Char8 (pack,
                              unpack)
import Crypto.Cipher.AES

import CryptoChallenge
import AESCBC

data Mode = ECB | Chaining
     deriving (Show, Eq)

-- Must cause a whole 16-byte block to be all the same
plaintext = pack (replicate (2 * aesBlocksize) '\NUL') 

{- A chosen-plaintext oracle for whether AES is in ECB mode or not -}
modeOracle :: (ByteString -> ByteString) -> Mode
modeOracle fn = if (hasDuplicateChunks aesBlocksize $ unpack $ fn plaintext)
                then ECB
                else Chaining

{-
        The sneaky deceiver
-}

key = pack $ "yellow submarine"
iv  = pack $ "green infinities" 

randomEncryptor :: ByteString -> ByteString
randomEncryptor plain = ecbEncrypt (initAES key) plain