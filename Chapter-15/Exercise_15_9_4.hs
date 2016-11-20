{-# OPTIONS_GHC -Wall #-}
module Exercise_15_9_4 where

fibs :: [Integer]
fibs = fibSeq 0 1
       where fibSeq x y = x : fibSeq y (x+y)