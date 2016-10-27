{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_2 where

sumDown :: Int -> Int
sumDown 0 = 0
sumDown n = n + sumDown (n-1)