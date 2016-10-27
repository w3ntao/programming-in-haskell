{-# OPTIONS_GHC -Wall #-}
module Exercise_6_8_6 where

import Prelude hiding (and, concat, replicate, (!!), elem)

and :: [Bool] -> Bool
and []       = True
and (x : xs) = if not x
                   then False
                   else and xs

concat :: [[a]] -> [a]
concat []       = []
concat (x : xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
xs !! 0 = (head xs)
xs !! n = (tail xs) !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x xs = if x == head xs
                then True
                else elem x (tail xs)