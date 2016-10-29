{-# OPTIONS_GHC -Wall #-}
module Exercise_8_9_2 where

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of EQ -> True
                                            LT -> occurs x l
                                            GT -> occurs x r