{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_9 where

scalarProduct :: [Int] ->[Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]