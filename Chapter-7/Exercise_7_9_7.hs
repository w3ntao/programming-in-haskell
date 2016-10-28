{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_7 where

import Data.Char

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chopN :: Int -> [Bit] -> [[Bit]]
chopN _ []   = []
chopN n bits = take n bits : chopN n (drop n bits)

chop8 :: [Bit] -> [[Bit]]
chop8 = chopN 8

chop9 :: [Bit] -> [[Bit]]
chop9 = chopN 9

parity :: [Bit] -> Bit
parity bits = sum bits `mod` 2

addParityBit :: [Bit] -> [Bit]
addParityBit bits = bits ++ [parity bits]

security :: [Bit] -> [Bit]
security = concat . (map addParityBit) . chop8

removeParityBit :: [Bit] -> [Bit]
removeParityBit bits = if last bits == parity (take 8 bits)
                           then take 8 bits
                           else error "Bit Error!"

desecurity :: [Bit] -> [Bit]
desecurity = concat . (map removeParityBit) . chop9

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = desecurity . security