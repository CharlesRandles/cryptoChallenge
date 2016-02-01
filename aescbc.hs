module AESCBC where

import Crypto.Cipher.AES
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString as B (ByteString,
                             take,
                             drop,
                             null,
                             empty,
                             append)
import Data.Bits (xor)

import CryptoChallenge
import XorEncode
import PKCS7

aesBlocksize :: Int
aesBlocksize = 0x10 -- Because the standard says so

toByteString :: Cipher -> B.ByteString
toByteString  c = pack (toString c)

fromByteString :: B.ByteString -> Cipher
fromByteString = fromString . unpack

cbcEncrypt :: AES -> ByteString -> ByteString -> ByteString
cbcEncrypt key vector plaintext
        | B.null plaintext = B.empty
        | otherwise = let cipher = encryptECB 
                                       key 
                                       (xorByteString 
                                           vector 
                                           (B.take aesBlocksize plaintext)) in
                             B.append cipher (cbcEncrypt key cipher (B.drop aesBlocksize plaintext))

cbcDecrypt :: AES -> ByteString -> ByteString -> ByteString
cbcDecrypt key vector ciphertext
           | B.null ciphertext = B.empty
           | otherwise = let cipherblock = (B.take aesBlocksize ciphertext) in
                             B.append 
                               (xorByteString (decryptECB key cipherblock) vector)
                               (cbcDecrypt key cipherblock (B.drop aesBlocksize ciphertext))
                             

key = initAES $ pack "YELLOW SUBMARINE"
vector = pack $ toString $  replicate aesBlocksize 0

plaintext = pack $ toString $ pkcs7 (fromString "Now is the winter of our discontent") aesBlocksize