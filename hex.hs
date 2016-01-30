{- Hexadecimal utilkity functions -}

module Hex (hexEncode,
            hexDecode,
            triples,
            ) where

import Numeric (readHex)
import Data.Char (ord,
                  chr)

fromHex :: String -> Int
fromHex = fst.head.readHex

hexChars :: String
hexChars = ['0'..'9'] ++ ['a'..'f']

hexDigitToChar :: Int -> Char
hexDigitToChar n = hexChars !! n

hexToString :: Int -> String
hexToString  = reverse . hexToString'
hexToString' n
        | n < 0x10 = (hexDigitToChar n):""
        | otherwise = hexDigitToChar (n `mod` 0x10) : hexToString (n `div` 0x10)

triples [] = []
triples (a:b:c:xs) = (a,b,c):triples xs

padEven s
        | length s `mod` 2 == 0 = s
        | otherwise = "0" ++ s

toPairs :: String -> [String]
toPairs = reverse . toPairs' [] . padEven
toPairs' l "" = l
toPairs' l (a:b:cs) = toPairs' ((a:b:[]):l) cs

hexToBytes :: String -> [Int]
hexToBytes s = map fromHex $ toPairs s

bytesToHex :: [Int] -> String
bytesToHex xs = concat $ map hexToString xs

hexEncodeString :: String -> String
hexEncodeString s = padEven $ bytesToHex $ map ord s

hexDecodeString :: String -> String
hexDecodeString s =  map chr (hexToBytes s)

hexEncode :: [Int] -> String
hexEncode = bytesToHex
hexDecode :: String -> [Int]
hexDecode = hexToBytes