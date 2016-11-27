{-# OPTIONS_GHC -Wall #-}
module Exercise_4_8_8 where

luhnDouble :: Int -> Int
luhnDouble x = y - (if y > 9 then 9 else 0)
               where y = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = sum [luhnDouble (x*2), luhnDouble y, luhnDouble (z*2), luhnDouble w] `mod` 10 == 0