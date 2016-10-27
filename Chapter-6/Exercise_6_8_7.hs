{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_7 where

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge xs ys = head smaller : merge (tail smaller) larger
              where (smaller, larger) = if head xs < head ys
                                            then (xs, ys)
                                            else (ys, xs)