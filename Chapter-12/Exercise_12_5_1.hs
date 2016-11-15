{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_1 where

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    fmap _  Leaf        = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)