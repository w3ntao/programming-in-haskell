{-# OPTIONS_GHC -Wall #-}
module Exercise_4_8_1 where

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2