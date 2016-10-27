{-# OPTIONS_GHC -Wall #-}
module Exercise_5_7_1 where

oneHundred :: [Int]
oneHundred = [ x^(2::Int) | x <- [1..100] ]