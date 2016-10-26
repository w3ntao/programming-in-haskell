{-# OPTIONS_GHC -Wall #-}
module Exercise_2_7_5 where

init0 :: [a] -> [a]
init0 list = reverse (tail (reverse list))

init1 :: [a] -> [a]
init1 [_]      = []
init1 (x : xs) = x : init1 xs