{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_10 where

import Exercise_7_9_9

luhnDouble :: Int -> Int
luhnDouble = (\x -> x + (if x > 9 then -9 else 0)) . (* 2)

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = sum (altMap luhnDouble id [x, y, z, w]) `mod` 10 == 0