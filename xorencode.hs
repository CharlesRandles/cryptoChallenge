module XorEncode where

import Data.Bits (xor)
import Data.ByteString as B (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import CryptoChallenge

xorEncode::Plain -> Key -> Cipher
xorEncode plain key = [xor (fst p) (snd p) | p <- zip plain (cycle key)]

xorEncodeText :: String -> String -> Cipher
xorEncodeText text key = xorEncode (fromString text) (fromString key)

xorByteString :: ByteString -> ByteString -> ByteString
xorByteString bs1 bs2 = pack $ toString $ xorEncodeText (unpack bs1) (unpack bs2)