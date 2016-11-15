{-# OPTIONS_GHC -Wall #-}
module Exercise_12_5_4 where

newtype ZipList a = Z [a]
                    deriving Show

instance Functor ZipList where
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    pure x = Z [x]
    Z gs <*> Z xs = Z [g x | g <- gs, x <- xs]