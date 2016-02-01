{- http://cryptopals.com/sets/1/challenges/1/ -}

module Base64 where

import Hex
import Data.Bits (shift, (.&.))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import CryptoChallenge

base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

tailPad :: String -> String
tailPad s= concat $ replicate ((length s `mod` 6) `div` 2) "=" 


encodeTriplet :: (Int, Int, Int) -> [Int]
encodeTriplet (a,b,c) = x:y:z:w:[]
             where x = shift (a .&. 0xfc) (-2)
                   y = (shift (a .&. 3) (4)) + (shift (b .&. 0xf0) (-4))
                   z = (shift (b .&. 0x0f) (2)) + (shift (c .&. 0xc0) (-6))
                   w = c .&. 0x3f
toTriplets :: [Int] -> [(Int, Int, Int)]
toTriplets [] = []
toTriplets (a:[]) = [(a,0,0)]
toTriplets (a:b:[]) = [(a,b,0)]
toTriplets (a:b:c:xs) = (a,b,c) : (toTriplets xs)

toBase64Char n  = base64Chars !! n

fromBase64Char c = fromJust $ elemIndex c base64Chars 

--base64Encode :: String -> String
base64Encode s =  (map toBase64Char 
                       $ concat 
                       [encodeTriplet t 
                         | t <- toTriplets (fromString s)])
                  ++ tailPad s

--base64 = base64Encode

----------------------------------
----- Decoder --------------------
----------------------------------

checkLength s
        | length s `mod` 4 == 0 = s
        | otherwise = error "Invalid Base64 string"

removeCRs = filter (\c -> c `elem` base64Chars)

base64Decode s = decodeString $ checkLength $ removeCRs s

{-
        ......|..V....|....V..|......|V
-}

decodeString "" = []
decodeString s = decodeQuad (take 4 s) ++ decodeString (drop 4 s)

decodeQuad (c1:c2:c3:c4:cs) =
       (shift v1 2) + (shift (v2 .&. 0x30) (-4)) :
       (shift (v2 .&. 0x0f) 4) + (shift (v3 .&. 0x3c) (-2)) :
       (shift (v3 .&. 0x03) 6) + v4 :
       []
       where v1 = fromBase64Char c1
             v2 = fromBase64Char c2
             v3 = fromBase64Char c3
             v4 = fromBase64Char c4