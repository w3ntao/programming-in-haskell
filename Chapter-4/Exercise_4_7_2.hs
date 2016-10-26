{-# OPTIONS_GHC -Wall #-}
module Exercise_4_7_2 where

third0 :: [a] -> a
third0 xs = head (tail (tail xs))

third1 :: [a] -> a
third1 = (!! 2)

third2 :: [a] -> a
third2 (_ : _ : x : _) = x