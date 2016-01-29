{- Some types and utilities for cryptography -}

module Crypto where

import Data.Char (ord, chr)

type Cipher = [Int]
type Key = [Int]
type Plain = [Int]

fromText = map ord
toText = map chr

charToKey :: Char -> [Int]
charToKey c = [ord c]