{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_6 where

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]
             where perfect y = sum (factors y) == 2*y
                   factors w = [ft | ft <- [1..w], w `mod` ft == 0]