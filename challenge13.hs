import CryptoChallenge

import KV
import Data.List.Split

import AES (encryptString, decryptString)

legalChar c = not $ c `elem` "=&"

sanitize :: String -> String
sanitize s = filter legalChar s

profileFor:: String -> String
profileFor email = toCookie $ KVMap [("email", (sanitize email)),
                                     ("role", "user")]

encryptProfile key iv email = encryptString key iv $ profileFor email

decryptProfile = decryptString

key = "green infinities"
iv= "yellow submarine"
email = "charles.randles@gmail.com"
secretString = encryptString key iv email