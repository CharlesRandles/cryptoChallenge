{-Key/Value pairs -}

module KV where

import Data.List (intercalate)
import Data.List.Split (splitOn)

type Pair = (String, String)

data KVMap = KVMap [Pair]

toJSON :: KVMap -> String
toJSON (KVMap ps) = "{" ++ (intercalate "," 
                                (map (\(k,v) -> k ++ ": '" ++ v ++"'") ps)) ++"}"

toCookie :: KVMap -> String
toCookie (KVMap ps) = intercalate "&" [k ++ "=" ++ v | (k,v) <- ps]
instance Show KVMap where
         show pairs = toJSON pairs

newMap :: KVMap
newMap = KVMap []

addPair :: KVMap -> Pair -> KVMap
addPair (KVMap ps) p = KVMap (p:ps)

ps = addPair (addPair newMap ("One", "First")) ("Two", "Second")

parseCookie :: String -> KVMap
parseCookie s = KVMap $ map firstTwo [splitOn "=" ps | ps <- splitOn "&" s]

firstTwo :: [String] -> Pair
firstTwo [] = ("","")
firstTwo (x:[]) = (x, "")
firstTwo (x:y:_) = (x,y)

cookie = "foo=bar&baz=quux&zap=zazzle&snee"
