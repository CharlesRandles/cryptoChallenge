{- Let's break some real crypto -}

import Crypto.Cipher.AES
import qualified Data.ByteString as B (ByteString, 
                             append,
                             take,
                             drop,
                             last)
import qualified Data.ByteString.Char8 as B8 (pack, unpack, length)
import qualified Data.ByteString.Lazy as L (pack)
import Data.Word
import Data.Char
import AES
import Base64
import CryptoChallenge
import ModeOracle
import PKCS7

constChar :: Char
constChar = 'A'

randomKey :: B.ByteString
randomKey = B8.pack $ toString $ Prelude.take aesBlocksize $ base64Decode "abcdabcdabcdabcdabcdabcd"

unknownText :: B.ByteString
unknownText = B8.pack $ toString $ base64Decode $ concat [
            "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg",
            "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq",
            "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg",
            "YnkK"]

encrypt :: B.ByteString -> B.ByteString
--encrypt str = ecbEncrypt (initAES randomKey) (B.append str unknownText)
encrypt str = encryptTail 0 str unknownText randomKey

encryptTail :: Int -> B.ByteString -> B.ByteString ->B.ByteString -> B.ByteString
encryptTail n sneakyText secretText key = 
            ecbEncrypt (initAES key) (B.append sneakyText (B.drop n secretText))

blockSize :: Int
blockSize = findBlockSize (B8.pack "")
findBlockSize :: B.ByteString -> Int
findBlockSize str = if lenCipher == lenNextCipher
                    then findBlockSize nextStr
                    else lenNextCipher - lenCipher
                    where lenCipher = B8.length (encrypt str)
                          lenNextCipher = B8.length (encrypt nextStr)
                          nextStr = B.append str (B8.pack [constChar])
                          
detectMode :: (B.ByteString -> B.ByteString) -> Mode
detectMode encrypt = if hasDuplicateChunks bs 
                           (B8.unpack $ encrypt 
                           (B8.pack $ replicate (bs * 3) constChar))
                     then ECB
                     else Chaining
                     where bs = blockSize

allChars :: [B.ByteString] 
allChars = [B8.pack (chr c:"") | c <- [0..255]]

shortBlock :: Int -> B.ByteString
shortBlock bs = B8.pack (replicate (bs - 1) constChar)

allBlocks :: Int -> [B.ByteString]
allBlocks n = [B.append (shortBlock blockSize) c | c <- allChars]


decryptDict = [(B.last str, B.take bs $ encrypt str) | str <- (allBlocks bs)]
            where bs = blockSize

nthChar n key secretText = [char | (char, cipher) <- decryptDict, cipher == ciphertext]
                             where ciphertext = B.take bs 
                                                 (encryptTail n 
                                                              (shortBlock bs)
                                                               secretText
                                                               key)
                                   bs = blockSize

decrypt ciphertext key = L.pack $ concat 
                         $ [nthChar n key ciphertext 
                           | n <- [0..(B8.length ciphertext)-1]]