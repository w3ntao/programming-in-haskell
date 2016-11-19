{-# OPTIONS_GHC -Wall #-}
module Exercise_14_5_4 where

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    fmap _  Leaf        = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
    foldl _ v  Leaf        = v
    foldl f v (Node l x r) = foldl f (foldl f (f v x) l) r

    foldr _ v  Leaf        = v
    foldr f v (Node l x r) = foldr f (foldr f (f x v) r) l

    foldMap _  Leaf        = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
    traverse g = sequenceA . fmap g