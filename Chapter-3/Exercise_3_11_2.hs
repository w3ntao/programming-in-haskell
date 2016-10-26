{-# OPTIONS_GHC -Wall #-}
module Exercise_3_11_2 where

bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[0]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x