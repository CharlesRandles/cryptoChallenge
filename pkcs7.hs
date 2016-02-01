module PKCS7 where
 
import CryptoChallenge
 
pkcs7 :: Cipher -> Int -> Cipher
pkcs7 cipher blocksize
  | padLength < 0  = error "pksc7: data longer than blocksize"
  | padLength > 0xff = error "pkcs7: can't pad more than 0xff bytes"
  | otherwise = cipher ++ replicate padLength padLength
    where padLength = (blocksize - ((length cipher) `mod` blocksize)) `mod` blocksize