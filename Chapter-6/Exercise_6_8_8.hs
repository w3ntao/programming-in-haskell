{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_8 where

import Exercise_6_8_7

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
            where (left, right) = halve xs