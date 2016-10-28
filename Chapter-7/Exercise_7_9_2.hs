{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_2 where

import Prelude hiding (all, any, takeWhile, dropWhile)

all :: (a -> Bool) -> [a] -> Bool
all p xs = length xs == (length . filter p) xs

any :: (a -> Bool) -> [a] -> Bool
any p = (> 0) . length . filter p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []       = []
takeWhile p (x : xs) = if p x
                           then x : takeWhile p xs
                           else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs) = if p x
                           then dropWhile p xs
                           else x : xs