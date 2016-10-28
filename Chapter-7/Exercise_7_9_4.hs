{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_4 where

dec2int :: [Int] -> Int
dec2int = foldl (\x xs -> 10*x + xs) 0