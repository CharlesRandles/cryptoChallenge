import System.Random
import Data.Char

import CryptoChallenge
import Base64
import AESCBC
import PKCS7

choice' :: [a]-> Int -> IO a
choice' xs n = return $ xs !! n

choice :: [a] -> IO a
choice xs = getStdRandom(randomR(0, (length xs)-1)) >>= choice' xs

randomByte :: IO Int
randomByte = getStdRandom(randomR(0,0xff))

randomBytes :: Int -> IO [Int]
randomBytes n = sequence $ take n $ repeat randomByte

randomKey :: IO String
randomKey = fmap toString $ randomBytes aesBlocksize

randomPlaintext :: IO String
randomPlaintext = choice plaintexts

plaintexts :: [String]
plaintexts = ["MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
              "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
              "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
              "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
              "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
              "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
              "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
              "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
              "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
              "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"]