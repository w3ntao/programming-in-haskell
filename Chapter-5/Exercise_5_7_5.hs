{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_5 where

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | z <- [1..n],
                       x <- [1..z-1],
                       y <- [1..z-1],
                       square x + square y == square z]
          where square = (^ (2::Int))