{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_2 where

grid :: Int -> Int -> [(Int, Int)]
grid n k = [(x, y) | x <- [0..n], y <- [0..k]]