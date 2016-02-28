module PKCS7 where
 
import CryptoChallenge
import Data.Char
 
pkcs7 :: Cipher -> Int -> Cipher
pkcs7 cipher blocksize
  | padLength < 0  = error "pksc7: data longer than blocksize"
  | padLength > 0xff = error "pkcs7: can't pad more than 0xff bytes"
  | otherwise = cipher ++ replicate padLength padLength
    where padLength = (blocksize - ((length cipher) `mod` blocksize)) `mod` blocksize

pkcs7String :: String -> Int -> String
pkcs7String cipher blocksize = toString $ pkcs7 (fromString cipher) blocksize

isValidPKCS7 :: String -> Bool
isValidPKCS7 s = all (\c -> (ord c) == n) (take n (reverse s))
                    where n = ord ((reverse s) !! 0)

unpackPKCS7 :: String -> String
unpackPKCS7 s
  | isValidPKCS7 s = reverse $ drop n $ reverse s
  | otherwise = error "Invalid PKCS7 packing"
    where n = ord $ ((reverse s) !!0)