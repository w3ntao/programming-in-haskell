{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_1 where

fac :: Int -> Int
fac n = if n <= 0
            then 1
            else n * fac (n-1)