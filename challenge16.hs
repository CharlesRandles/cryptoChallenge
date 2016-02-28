import AES
import CryptoChallenge

import Data.Bits
import Data.Char

flipBit byte n = xor byte (shiftL 1 n)
flipChar c = chr (flipBit (ord c) 1)


key = take 16 $ cycle "key"
iv = take 16 $ cycle "iv"
plaintext = take 128 $ cycle "0123456789abcdef"

ciphertext = encryptString key iv plaintext

munged = (flipChar (head ciphertext)) : (tail ciphertext)

output = decryptString key iv munged

allBytes = [0..0xff]

--setByte :: String -> Int -> Char -> String
setByte text pos value = (take pos text)
                         ++ value:[]        
                         ++ drop (pos + 1) text

allCiphertexts ciphertext pos =
               [(chr value, setByte ciphertext pos (chr value)) | value <- allBytes]

attackByte key iv ciphertext target pos = fst $ head $
             filter (\(c,s) -> s!!(pos + aesBlocksize) == target)
                  [((fst ct), decryptString key iv (snd ct))
                        | ct <- allCiphertexts ciphertext pos]

--This is terrifying.
attack key iv ciphertext target =
       [attackByte key iv ciphertext (target!!pos) pos | pos <- [0..(length target) -1]]
       ++ (drop (length target) ciphertext)

{-
I want my victim to see the words "chunky bacon"
So: We capture a ciphertext, and alter it so
the second block decrypts to our target
-} 
                 
--target = ";admin=true;"
target = "chunky bacon"
attacked_ciphertext = attack key iv ciphertext target
{- and we send that merrily on its way to the unsuspecting victim
who decrypts the text happily-}
clear = decryptString key iv attacked_ciphertext