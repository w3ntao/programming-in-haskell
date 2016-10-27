{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_10 where

import Data.Char

let2int :: Char -> Char -> Int
let2int c base = ord c - ord base

int2let :: Int -> Char -> Char
int2let n base = chr (ord base + n)

shift :: Int -> Char -> Char
shift n c | isLower c = trans 'a'
          | isUpper c = trans 'A'
          | otherwise = c
          where trans base = int2let ((let2int c base + n) `mod` 26) base

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]