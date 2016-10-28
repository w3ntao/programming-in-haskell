{-# OPTIONS_GHC -Wall #-}
module Exercise_7_9_3 where

import Prelude hiding (map, filter)

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then (x : xs) else xs) []