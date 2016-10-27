{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_3 where

import Exercise_5_7_2

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]