{-# OPTIONS_GHC -Wall #-}
module Exercise_8_9_4 where

import Exercise_8_9_3

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance l) (balance r)
              where (l, r) = halve xs