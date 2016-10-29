{-# OPTIONS_GHC -Wall #-}
module Exercise_8_9_5 where

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)