{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_7 where

listComprehension :: [(Int, Int)]
listComprehension = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]