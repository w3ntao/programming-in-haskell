{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_4 where

euclid :: Int -> Int -> Int
euclid x y = if x == y
                 then x
                 else euclid smaller (larger - smaller)
             where (smaller, larger) = if x > y
                                           then (y, x)
                                           else (x, y)