{-# OPTIONS_GHC -Wall #-}
module Exercise_1_7_4 where

qsort :: Ord t => [t] -> [t]
qsort []       = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
                 where smaller = [a | a <- xs, a <= x]
                       larger  = [b | b <- xs, b >  x]